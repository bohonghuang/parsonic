(in-package #:parsonic)

(defgeneric expand-expr/compile (op &rest args)
  (:method (op &rest args)
    (apply #'expand-expr op args)))

(defvar *expand/compile-env* nil)
(defvar *expand/compile-known* nil)
(defvar *expand/compile-cache* nil)

(defstruct lexical-arg
  (parser #'values :type function)
  (binding nil :type list))

(defun lexical-arg-name (arg)
  (first (lexical-arg-binding arg)))

(defun lexical-arg-parser-p (arg)
  (apply #'eq (lexical-arg-binding arg)))

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

(defun parser-arg (name value)
  (with-gensyms (lexical)
    (let (arg)
      (setf (get lexical 'lexical-store) t
            arg (make-parser-arg
                 :parser (let ((parent-env *expand/compile-env*)
                               (parent-known *expand/compile-known*))
                           (let ((cache (make-hash-table :test #'eq)))
                             (lambda (&optional (function *expand*))
                               (setf (second (parser-arg-binding arg)) (first (parser-arg-binding arg)))
                               (ensure-gethash
                                function cache
                                (let ((*expand/compile-env* parent-env)
                                      (*expand/compile-known* parent-known))
                                  (funcall function value))))))
                 :binding (list name lexical))))))

(defstruct (curry-arg (:include lexical-arg)))

(defmethod lexical-arg-send ((arg curry-arg))
  (curry-arg-binding arg))

(defmethod lexical-arg-receive ((arg curry-arg)))

(defun curry-arg (value)
  (with-gensyms (curry)
    (let (arg)
      (setf (get curry 'lexical-store) t
            arg (make-curry-arg
                 :parser (let ((parent-env *expand/compile-env*)
                               (parent-known *expand/compile-known*))
                           (lambda (&optional (function *expand*))
                             (setf (second (curry-arg-binding arg)) (first (curry-arg-binding arg)))
                             (let ((*expand/compile-env* parent-env)
                                   (*expand/compile-known* parent-known))
                               (funcall function value))))
                 :binding (list curry value))))))

(defun lexical-arg (name)
  (find name *expand/compile-env* :key #'lexical-arg-name))

(defgeneric expand/compile (form)
  (:method ((expr list))
    (let ((*expand* #'expand/compile))
      (apply #'expand-expr/compile expr)))
  (:method ((symbol symbol))
    (assert (not (keywordp symbol)))
    (if-let ((arg (lexical-arg symbol)))
      (funcall (lexical-arg-parser arg))
      symbol)))

(defun receive-lexical-env (form &optional (symbols nil symbolsp))
  (let ((lexical-env
          (loop :for arg :in *expand/compile-env*
                :for binding := (lexical-arg-receive arg)
                :when binding
                  :unless (member (first binding) bindings :key #'first)
                    :when (or (not symbolsp) (member (first binding) symbols))
                      :collect binding :into bindings
                :finally (return bindings))))
    `(parser/let ,lexical-env ,form)))

(defun send-lexical-env (form env)
  (if-let ((lexical-env (loop :for arg :in env
                              :for binding := (lexical-arg-send arg)
                              :when binding
                                :collect binding)))
    `(parser/let ,lexical-env ,form)
    form))

(defun form-symbol-set (form)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((recur (form)
               (typecase form
                 (cons (recur (car form)) (recur (cdr form)))
                 (symbol (setf (gethash form table) t)))))
      (recur form))
    table))

(defun filter-lexical-env (form predicate)
  (destructuring-case form
    ((parser/let bindings body)
     (let ((bindings (loop :for (var val) :in bindings
                           :when (funcall predicate var)
                             :collect (list var val))))
       (if bindings `(parser/let ,bindings ,body) body)))
    ((t &rest args) (declare (ignore args)) form)))

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
      (let ((form (remove-intermediates (expand form))))
        (case (car form)
          ((curry rcurry) (gensym (symbol-name (car form))))
          (t (recur form)))))))

(defun lambda-list-lexical-args (lambda-list-args)
  (loop :with sequential-binding-p := nil
        :for (name . value) :in lambda-list-args
        :if (member name lambda-list-keywords)
          :do (setf sequential-binding-p t)
        :else
          :append (let ((*expand/compile-env* (if sequential-binding-p (append lexical-args *expand/compile-env*) *expand/compile-env*)))
                    (labels ((recur (value &optional (arg (curry #'parser-arg name)))
                               (typecase value
                                 ((cons (member curry rcurry) list)
                                  (multiple-value-bind (car-args cdr-args)
                                      (loop :for arg :in (cdr value)
                                            :for (car . cdr) := (recur arg #'curry-arg)
                                            :collect car :into car-args
                                            :append cdr :into cdr-args
                                            :finally (return (values car-args cdr-args)))
                                    (cons (funcall arg (cons (car value) car-args)) (append car-args cdr-args))))
                                 (t (list (funcall arg value))))))
                      (recur value)))
            :into lexical-args
        :finally (return lexical-args)))

(defun collect-parser-args-expand (object)
  (typecase object
    (symbol
     (if-let ((arg (lexical-arg object)))
       (funcall (lexical-arg-parser arg))
       object))
    (list
     (catch 'collect-parser-args
       (unless (member (car object) *expand/compile-known* :key #'caar)
         (apply #'expand-expr/compile object))))))

(defun collect-parser-args (name lambda-list-args body)
  (let ((lexical-args (lambda-list-lexical-args lambda-list-args)))
    (flet ((collect-parser-args ()
             (let ((*expand* #'collect-parser-args-expand)
                   (*expand/compile-env* lexical-args)
                   (*expand/compile-known* (cons (cons (cons name nil) :collect) *expand/compile-known*)))
               (expand body)
               (nconc
                (loop :for (name . nil) :in lambda-list-args
                      :for arg := (lexical-arg name)
                      :unless (member name lambda-list-keywords)
                        :collect (cons name (lexical-arg-parser-p arg)))
                (loop :for arg :in lexical-args
                      :when (curry-arg-p arg)
                        :sum 1 :into name
                        :and :collect (cons name (lexical-arg-parser-p arg)))))))
      (let ((arg-info (ensure-gethash name *expand/compile-cache* (collect-parser-args))))
        (if (find :collect *expand/compile-known* :key #'cdr)
            (throw 'collect-parser-args
              (loop :for arg :in lexical-args
                    :when (parser-arg-p arg)
                      :when (assoc-value arg-info (lexical-arg-name arg))
                        :do (funcall (parser-arg-parser arg))
                    :when (curry-arg-p arg)
                      :sum 1 :into name
                      :and :when (assoc-value arg-info name)
                             :do (funcall (curry-arg-parser arg))))
            (values arg-info lexical-args))))))

(defun recursive-unit-name (definition)
  (destructuring-bind (name &rest args) definition
    (declare (ignore args))
    (ensure-gethash
     definition *expand/compile-cache*
     (let ((name (gensym (symbol-name name))))
       (setf (get name 'parser) name)
       name))))

(defun recursive-unit-name-p (name)
  (destructuring-bind (name &rest parsers) name
    (declare (ignore parsers))
    (when (and (symbolp name) (get name 'parser))
      name)))

(defun %expand-expr/compile (name lambda-list args body &aux (*expand/compile-cache* (or *expand/compile-cache* (make-hash-table :test #'equal))))
  (let ((lambda-list-args (loop :for (name default-value) :in (mapcar #'ensure-list lambda-list)
                                :collect (cons name (if-let ((cons (assoc name args))) (cdr cons) default-value)))))
    (multiple-value-bind (arg-info lexical-args) (collect-parser-args name lambda-list-args body)
      (filter-lexical-env
       (receive-lexical-env
        (if-let ((fdef (loop :for fdef :in *expand/compile-known*
                             :for ((fname . fargs) . nil) := fdef
                             :when (eq name fname)
                               :when (loop :for (name . value) :in fargs
                                           :do (assert (parser-tree-p value))
                                           :always (equal (parser-signature (assoc-value lambda-list-args name)) value))
                                 :return fdef)))
          (let ((fname (etypecase (cdr fdef)
                         (boolean (setf (cdr fdef) (recursive-unit-name (car fdef))))
                         (symbol (cdr fdef)))))
            `(parser/call ,fname . ,(mapcar #'cdr (remove-if (rcurry #'member (cdar fdef) :key #'car) args :key #'car))))
          (let* ((parser-arg-names (mapcar #'car (remove-if-not #'cdr arg-info)))
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
                               (*expand/compile-known* known))
                           (expand body)))
                 (result (if-let ((args (remove-if #'curry-arg-p lexical-args)))
                           (send-lexical-env result args)
                           result))
                 (fname (cdr (first known)))
                 (signature (list (cons (or fname name) (mapcar #'cdr parsers)) lambda-list))
                 (result `(parser/unit ,signature ,result))
                 (bindings (loop :for arg :in lambda-list
                                 :unless (member arg lambda-list-keywords)
                                   :collect (etypecase arg
                                              ((cons symbol (cons t null))
                                               (cons (car arg) (or (assoc-value variables (car arg)) (cdr arg))))
                                              (symbol
                                               (cons arg (or (assoc-value variables arg) (list nil)))))))
                 (result `(parser/let ,bindings ,result))
                 (result (if-let ((args (remove-if-not #'curry-arg-p lexical-args)))
                           (send-lexical-env result args)
                           result)))
            (assert result)
            result)))
       (rcurry #'gethash (form-symbol-set (mapcar #'cdr (remove-if (curry #'assoc-value arg-info) args :key #'car))))))))

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
           (walk-parsers-in-lambda (lambda (parser) (send-lexical-env (expand parser) args)) function)))
      ,(expand parser))))

(defmethod expand-expr/compile ((op (eql 'parser-call)) &rest args)
  (destructuring-bind (function &rest args &aux (env *expand/compile-env*)) args
    (labels ((recur (object largs rargs)
               (etypecase object
                 (lexical-arg
                  (funcall (lexical-arg-parser object) (rcurry #'recur largs rargs)))
                 ((and symbol (not null))
                  (recur (lexical-arg object) largs rargs))
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
                                                  env))
                           (args (loop :for arg :in (append largs rargs)
                                       :if (curry-arg-p arg)
                                         :collect (lexical-arg-name arg)
                                       :else
                                         :collect arg)))
                       (expand `(,function . ,args)))))))))
      (recur function nil args))))
