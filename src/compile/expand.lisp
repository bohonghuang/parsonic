(in-package #:parseq)

(defgeneric expand-expr/compile (op &rest args)
  (:method (op &rest args)
    (apply #'expand-expr op args)))

(defvar *expand/compile-env* nil)
(defvar *expand/compile-known* nil)
(defvar *expand/compile-args* nil)

(defstruct parser-arg
  (parser #'values :type function)
  (binding nil :type list))

(defgeneric expand/compile (form)
  (:method ((expr list))
    (let ((*expand* #'expand/compile))
      (apply #'expand-expr/compile expr)))
  (:method ((symbol symbol))
    (assert (not (keywordp symbol)))
    (if-let ((cons (assoc symbol *expand/compile-env*)))
      (funcall (parser-arg-parser (cdr cons)))
      symbol)))

(defun receive-lexical-env (form)
  (if-let ((lexical-env
            (loop :for (name . arg) :in *expand/compile-env*
                  :for binding := (parser-arg-binding arg)
                  :unless (member (first binding) bindings :key #'first)
                    :collect binding :into bindings
                  :finally (return bindings))))
    `(parser/let nil ,lexical-env ,form)
    form))

(defun send-lexical-env (form env)
  (if-let ((lexical-env (loop :for (name . arg) :in env
                              :for (var val) := (parser-arg-binding arg)
                              :collect (list val var))))
    `(parser/let nil ,lexical-env ,form)
    form))

(defun %expand-expr/compile (name lambda-list args body)
  (receive-lexical-env
   (if-let ((fdef (loop :for fdef :in *expand/compile-known*
                        :for ((fname . fargs) . nil) := fdef
                        :when (eq name fname)
                          :when (loop :for (name . value) :in fargs
                                      :always (equal (assoc-value args name) value))
                            :return fdef)))
     (let ((fname (etypecase (cdr fdef)
                    (boolean (setf (cdr fdef) (gensym (string name))))
                    (symbol (cdr fdef)))))
       (loop :for caller-fdef :in *expand/compile-known*
             :until (eq fdef caller-fdef)
             :unless (cdr caller-fdef)
               :do (setf (cdr caller-fdef) t))
       `(parser/call ,fname . ,(mapcar #'cdr (remove-if (rcurry #'member (cdar fdef) :key #'car) args :key #'car))))
     (let ((parser-arg-names nil)
           (parser-arg-cache (or *expand/compile-args* (make-hash-table :test #'eq))))
       (flet ((parser-arg (name value &optional (parent-env *expand/compile-env*))
                (with-gensyms (lexical)
                  (let (arg)
                    (setf (get lexical 'lexical-store) t
                          arg (make-parser-arg
                               :parser (lambda ()
                                         (pushnew name parser-arg-names)
                                         (setf (second (parser-arg-binding arg)) (first (parser-arg-binding arg)))
                                         (let ((*expand/compile-env* parent-env))
                                           (expand/compile value)))
                               :binding (list name lexical)))))))
         (let* ((lambda-list-args
                  (loop :with sequential-binding-p := nil
                        :for (name default-value) :in (mapcar #'ensure-list lambda-list)
                        :for value := (if-let ((cons (assoc name args))) (cdr cons) default-value)
                        :if (member name lambda-list-keywords)
                          :do (setf sequential-binding-p t)
                        :else
                          :collect (cons name
                                         (let ((*expand/compile-env* (if sequential-binding-p
                                                                         (append lambda-list-args *expand/compile-env*)
                                                                         *expand/compile-env*)))
                                           (parser-arg name value)))
                            :into lambda-list-args
                        :finally (return lambda-list-args)))
                (arg-info (ensure-gethash
                           name parser-arg-cache
                           (let ((*expand/compile-env* lambda-list-args)
                                 (*expand/compile-known* (cons (cons (cons name args) nil) *expand/compile-known*))
                                 (*expand/compile-args* parser-arg-cache))
                             (expand/compile body)
                             (loop :for arg :in lambda-list
                                   :for (name) := (ensure-list arg)
                                   :collect (cons name (member name parser-arg-names))))))
                (parser-arg-names (mapcar #'car (remove-if-not #'cdr arg-info)))
                (parsers (mapcar (lambda (name) (cons name (assoc-value args name))) parser-arg-names))
                (lambda-list (loop :for arg :in lambda-list
                                   :unless (member (car (ensure-list arg)) parser-arg-names)
                                     :collect arg))
                (variables (loop :for (name . arg) :in args
                                 :unless (member name parser-arg-names)
                                   :collect (list name arg)))
                (known (cons (cons (cons name parsers) nil) *expand/compile-known*))
                (result (let ((*expand/compile-env* lambda-list-args)
                              (*expand/compile-known* known)
                              (*expand/compile-args* parser-arg-cache))
                          (expand/compile body)))
                (signature (list (cons name parsers) lambda-list))
                (lambda-list (if (intersection lambda-list lambda-list-keywords)
                                 (append lambda-list '(&initial) variables)
                                 (progn
                                   (assert (equal lambda-list (mapcar #'first variables)))
                                   variables)))
                (result (send-lexical-env result lambda-list-args))
                (fname (cdr (first known)))
                (result (if fname result `(parser/unit ,signature ,result)))
                (result (etypecase fname
                          (boolean (if lambda-list `(parser/let nil ,lambda-list ,result) result))
                          (symbol `(parser/let ,fname ,lambda-list ,result))))
                (result result))
           (assert result)
           result))))))

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
                               :collect (cons name (with-gensyms (lexical)
                                                     (setf (get lexical 'lexical-store) t)
                                                     (make-parser-arg
                                                      :parser (curry #'error 'error)
                                                      :binding (list name lexical))))))
                 (*expand/compile-env* #+nil nil #-nil (append args *expand/compile-env*)))
            (labels ((walk (form)
                       (typecase form
                         (null form)
                         (proper-list
                          (destructuring-case form
                            ((parser parser) `(with-codegen ,(#+nil prog1 #-nil send-lexical-env (expand/compile parser) args)))
                            ((t &rest args) (cons (car form) (mapcar #'walk args)))))
                         (t form))))
              (walk function))))
       ,(expand parser)))))
