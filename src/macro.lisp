(in-package #:parsonic)

(defun ordinary-lambda-list (lambda-list)
  (loop :with in-optional-args-p
        :for arg :in lambda-list
        :when (member arg lambda-list-keywords)
          :do (setf in-optional-args-p t)
        :if in-optional-args-p
          :collect arg
        :else
          :collect (etypecase arg
                     (symbol arg)
                     (list (car arg)))))

(defun lambda-list-arguments (lambda-list)
  (multiple-value-bind (required optional rest key allow-other-keys aux keyp)
      (parse-ordinary-lambda-list (ordinary-lambda-list lambda-list))
    (declare (ignore allow-other-keys aux keyp))
    (values (nconc required (mapcar #'car optional) (mapcan #'car key) (when rest (list rest))) (if rest 'apply 'funcall))))

(defun defparser/eval (name parser args)
  (let ((parser (if (boundp name)
                    (apply (symbol-value name) args)
                    (let ((self #'values))
                      (progv (list name)
                          (list (lambda (&rest parser-args)
                                  (if (equal args parser-args)
                                      (lambda (input) (funcall self input))
                                      (lambda (input) (funcall (apply name args) input)))))
                        (setf self (funcall parser))))))
        (name (parser-symbol-name name)))
    (lambda (input &aux (position (input-position/eval input)) (result (parser-call parser input)))
      (if (parse-failure-p result)
          (parse-failure (cons `((,name) ,(parse-failure result)) position))
          result))))

(defmacro defparser (name lambda-list &body body)
  (let ((docstring (when (stringp (car body)) (pop body)))
        (body (if (= (length body) 1) (first body) `(progn . ,body))))
    `(progn
       ,(let ((name (parser-name-symbol name))
              (args (lambda-list-arguments lambda-list)))
          `(defun ,name ,(loop :for arg :in lambda-list
                               :for (name value) := (ensure-list arg)
                               :if value
                                 :collect `(,name (parser ,value))
                               :else
                                 :collect arg)
             ,@(when docstring (list docstring))
             (defparser/eval ',name (lambda () (parser ,body)) (list . ,args))))
       ,(with-gensyms (op args)
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defmethod expand-expr/compile ((,op (eql ',name)) &rest ,args)
               ,@(when docstring (list docstring))
               (destructuring-bind ,(loop :for arg :in lambda-list
                                          :for (name value) := (ensure-list arg)
                                          :if value
                                            :collect `(,name (quote ,value))
                                          :else
                                            :collect arg)
                   ,args
                 (%expand-expr/compile
                  ',name
                  ',lambda-list
                  (subseq
                   (list . ,(loop :for variable :in (lambda-list-arguments lambda-list) :collect `(cons ',variable ,variable)))
                   0 (length ,args))
                  ',body))))))))
