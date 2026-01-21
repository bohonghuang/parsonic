(in-package #:parsonic)

(defgeneric expand-expr/compile (op &rest args)
  (:method (op &rest args)
    (apply #'expand-expr op args)))

(defvar *expand/compile-env* nil)
(defvar *expand/compile-known* nil)
(defvar *expand/compile-args* nil)

(defstruct lexical-arg
  (parser #'values :type function)
  (binding nil :type list))

(defun lexical-arg-name (arg)
  (first (lexical-arg-binding arg)))

(defgeneric lexical-arg-send (arg))

(defgeneric lexical-arg-receive (arg))

(defstruct (parser-arg (:include lexical-arg)))

(defmethod lexical-arg-send ((arg parser-arg))
  (reverse (parser-arg-binding arg)))

(defmethod lexical-arg-receive ((arg parser-arg))
  (parser-arg-binding arg))

(defun parser-tree-p (object)
  (typecase object
    ((cons symbol t)
     (eql (search (string '#:parser/) (symbol-name (car object))) 0))))

(defun parser-arg (name value &optional (parser-arg-names (cons nil nil)))
  (with-gensyms (lexical)
    (let (arg)
      (setf (get lexical 'lexical-store) t
            arg (make-parser-arg
                 :parser (let ((parent-env *expand/compile-env*))
                           (lambda (&optional (function #'expand/compile))
                             (pushnew name (cdr parser-arg-names))
                             (setf (second (parser-arg-binding arg)) (first (parser-arg-binding arg)))
                             (let ((*expand/compile-env* parent-env))
                               (funcall function value))))
                 :binding (list name lexical))))))

(defstruct (curry-arg (:include lexical-arg)))

(defmethod lexical-arg-send ((arg curry-arg))
  (curry-arg-binding arg))

(defmethod lexical-arg-receive ((arg curry-arg))
  nil)

(defun curry-arg (value)
  (with-gensyms (curry)
    (let (arg)
      (setf arg (let ((parent-env *expand/compile-env*))
                  (make-curry-arg
                   :parser (lambda (&optional (function #'expand/compile))
                             (setf (second (curry-arg-binding arg)) (first (curry-arg-binding arg)))
                             (let ((*expand/compile-env* parent-env))
                               (funcall function value)))
                   :binding (list curry value)))))))

(defgeneric expand/compile (form)
  (:method ((expr list))
    (let ((*expand* #'expand/compile))
      (apply #'expand-expr/compile expr)))
  (:method ((symbol symbol))
    (assert (not (keywordp symbol)))
    (if-let ((arg (find symbol *expand/compile-env* :key #'lexical-arg-name)))
      (funcall (lexical-arg-parser arg))
      symbol)))

(defun receive-lexical-env (form)
  (if-let ((lexical-env
            (loop :for arg :in *expand/compile-env*
                  :for binding := (lexical-arg-receive arg)
                  :when binding
                    :unless (member (first binding) bindings :key #'first)
                      :collect binding :into bindings
                  :finally (return bindings))))
    `(parser/let nil ,lexical-env ,form)
    form))

(defun send-lexical-env (form env)
  (if-let ((lexical-env (loop :for arg :in env
                              :for binding := (lexical-arg-send arg)
                              :when binding
                                :collect binding)))
    `(parser/let nil ,lexical-env ,form)
    form))

(defun remove-intermediates (form)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda #'remove-intermediates function) . ,(mapcar #'remove-intermediates parsers)))
        (((parser/unit parser/let) &rest args)
         (remove-intermediates (lastcar args)))
        ((t &rest args) (cons (car form) (mapcar #'remove-intermediates args))))
      form))

(defun parser-signature (form)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((recur (form)
               (typecase form
                 (symbol (if (symbol-package form) form (ensure-gethash form table (1+ (hash-table-count table)))))
                 (cons (cons (recur (car form)) (recur (cdr form))))
                 (t form))))
      (recur (remove-intermediates (expand/compile form))))))

(defmethod expand-expr/compile ((op (eql 'curry)) &rest args)
  (declare (ignore args)))

(defmethod expand-expr/compile ((op (eql 'rcurry)) &rest args)
  (declare (ignore args)))

(defun %expand-expr/compile (name lambda-list args body)
  (receive-lexical-env
   (let ((lambda-list-args (loop :for (name default-value) :in (mapcar #'ensure-list lambda-list)
                                 :collect (cons name (if-let ((cons (assoc name args))) (cdr cons) default-value)))))
     (if-let ((fdef (loop :for fdef :in *expand/compile-known*
                          :for ((fname . fargs) . nil) := fdef
                          :when (eq name fname)
                            :when (loop :for (name . value) :in fargs
                                        :always (if (parser-tree-p value)
                                                    (equal (parser-signature (assoc-value lambda-list-args name)) value)
                                                    (equal (assoc-value lambda-list-args name) value)))
                              :return fdef)))
       (let ((fname (etypecase (cdr fdef)
                      (boolean (setf (cdr fdef) (gensym (string name))))
                      (symbol (cdr fdef)))))
         (loop :for caller-fdef :in *expand/compile-known*
               :until (eq fdef caller-fdef)
               :unless (cdr caller-fdef)
                 :do (setf (cdr caller-fdef) t))
         `(parser/call ,fname . ,(mapcar #'cdr (remove-if (rcurry #'member (cdar fdef) :key #'car) args :key #'car))))
       (Let ((parser-arg-names (cons nil nil))
             (parser-arg-cache (or *expand/compile-args* (make-hash-table :test #'eq))))
         (let* ((lexical-args
                  (loop :with sequential-binding-p := nil
                        :for (name . value) :in lambda-list-args
                        :if (member name lambda-list-keywords)
                          :do (setf sequential-binding-p t)
                        :else
                          :append (let ((*expand/compile-env* (if sequential-binding-p (append lexical-args *expand/compile-env*) *expand/compile-env*)))
                                    (labels ((recur (name value)
                                               (typecase value
                                                 ((cons (member curry rcurry) list)
                                                  (let ((curry-args (loop :for arg :in (cdr value) :collect (curry-arg arg))))
                                                    (cons (parser-arg name (cons (car value) curry-args) parser-arg-names) curry-args)))
                                                 (t (list (parser-arg name value parser-arg-names))))))
                                      (recur name value)))
                            :into lexical-args
                        :finally (return lexical-args)))
                (arg-info (ensure-gethash
                           name parser-arg-cache
                           (let ((*expand/compile-env* lexical-args)
                                 (*expand/compile-known* (cons (cons (cons name args) nil) *expand/compile-known*))
                                 (*expand/compile-args* parser-arg-cache))
                             (expand/compile body)
                             (loop :for arg :in lambda-list
                                   :for (name) := (ensure-list arg)
                                   :collect (cons name (when (member name parser-arg-names) t))))))
                (parser-arg-names (mapcar #'car (remove-if-not #'cdr arg-info)))
                (parsers (loop :for (name . value) :in lambda-list-args
                               :when (member name parser-arg-names)
                                 :collect (cons name (parser-signature value))))
                (lambda-list (loop :for arg :in lambda-list
                                   :unless (member (car (ensure-list arg)) parser-arg-names)
                                     :collect arg))
                (variables (loop :for (name . value) :in args
                                 :unless (member name parser-arg-names)
                                   :collect (list name value)))
                (known (cons (cons (cons name parsers) nil) *expand/compile-known*))
                (result (let ((*expand/compile-env* lexical-args)
                              (*expand/compile-known* known)
                              (*expand/compile-args* parser-arg-cache))
                          (expand/compile body)))
                (signature (when (every #'cdr parsers) (list (cons name (mapcar #'cdr parsers)) lambda-list)))
                (lambda-list (if (intersection lambda-list lambda-list-keywords)
                                 (append lambda-list '(&initial) variables)
                                 (progn
                                   (assert (equal lambda-list (mapcar #'first variables)))
                                   variables)))
                (result (send-lexical-env result lexical-args))
                (fname (cdr (first known)))
                (result (if (or fname (null signature)) result `(parser/unit ,signature ,result)))
                (result (etypecase fname
                          (boolean (if lambda-list `(parser/let nil ,lambda-list ,result) result))
                          (symbol `(parser/let ,fname ,lambda-list ,result))))
                (result result))
           (assert result)
           result))))))

(defun walk-parsers-in-lambda (function form)
  (typecase form
    (null form)
    (proper-list
     (destructuring-case form
       (((with-codegen parser) parser)
        `(with-codegen ,(funcall function parser)))
       ((t &rest args) (cons (car form) (mapcar (curry #'walk-parsers-in-lambda function) args)))))
    (t form)))

(defmethod expand-expr/compile ((op (eql 'apply)) &rest args)
  (destructuring-bind (function parser) args
    (receive-lexical-env
     `(parser/apply
       ,(destructuring-bind (lambda lambda-list &rest body) function
          (declare (ignore body))
          (assert (eq lambda 'lambda))
          (let* ((args (loop :for arg :in lambda-list
                             :for (name) := (ensure-list arg)
                             :unless (member name lambda-list-keywords)
                               :collect (with-gensyms (lexical)
                                          (setf (get lexical 'lexical-store) t)
                                          (make-parser-arg
                                           :parser (curry #'error 'error)
                                           :binding (list name lexical)))))
                 (*expand/compile-env* (append args *expand/compile-env*)))
            (walk-parsers-in-lambda (lambda (parser) (send-lexical-env (expand/compile parser) args)) function)))
       ,(expand parser)))))

(defmethod expand-expr/compile ((op (eql 'parser-call)) &rest args)
  (destructuring-bind (function &rest args) args
    (labels ((recur (object largs rargs)
               (etypecase object
                 (lexical-arg
                  (funcall (lexical-arg-parser object) (rcurry #'recur largs rargs)))
                 ((and symbol (not null))
                  (recur (find object *expand/compile-env* :key #'lexical-arg-name) largs rargs))
                 (cons
                  (destructuring-ecase object
                    ((curry function &rest args)
                     (recur function (append args largs) rargs))
                    ((rcurry function &rest args)
                     (recur function largs (append rargs args)))
                    ((function function)
                     (let ((*expand/compile-env* (nconc
                                                  (loop :for arg :in (append largs rargs)
                                                        :when (curry-arg-p arg)
                                                          :collect arg)
                                                  *expand/compile-env*))
                           (args (loop :for arg :in (append largs rargs)
                                       :if (curry-arg-p arg)
                                         :collect (lexical-arg-name arg)
                                       :else
                                         :collect arg)))
                       (expand/compile `(,function . ,args)))))))))
      (recur function nil args))))
