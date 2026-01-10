(in-package #:parseq)

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

(defmacro defparser (name lambda-list &body body)
  (let ((body (if (= (length body) 1) (first body) `(progn . ,body))))
    `(progn
       ,(let ((name (parser-name-symbol name)))
          (with-gensyms (self input args)
            (multiple-value-bind (funcall-args funcall) (lambda-list-arguments lambda-list)
              `(defun ,name ,lambda-list
                 (if (boundp ',name)
                     (,funcall (symbol-value ',name) . ,funcall-args)
                     (let* ((,self #'values)
                            (,name (lambda (&rest ,args)
                                     (if (equal ,args (,funcall #'list . ,funcall-args))
                                         (lambda (,input) (funcall ,self ,input))
                                         (lambda (,input) (funcall (,funcall #',name . ,funcall-args) ,input))))))
                       (declare (type function ,self) (special ,name))
                       (setf ,self (parser ,body))))))))
       ,(with-gensyms (op args)
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defmethod expand-expr/compile ((,op (eql ',name)) &rest ,args)
               (destructuring-bind ,lambda-list ,args
                 (%expand-expr/compile
                  ',name
                  ',lambda-list
                  (subseq
                   (list . ,(loop :for variable :in (lambda-list-arguments lambda-list) :collect `(cons ',variable ,variable)))
                   0 (length ,args))
                  ',body))))))))
