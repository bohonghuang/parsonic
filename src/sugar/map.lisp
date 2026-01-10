(in-package #:parseq)

(defmacro for (bindings &body body)
  (declare (ignore bindings body)))

(defmethod expand-expr ((op (eql 'for)) &rest args)
  (destructuring-bind (bindings &rest body) args
    (with-gensyms (args)
      (expand
       `(funcall
         (lambda (,args)
           (parser/constantly
            ,(multiple-value-bind (names ignored)
                 (loop :for (name) :in bindings
                       :for gensym := (gensym)
                       :if name
                         :collect name :into names
                       :else
                         :collect gensym :into names
                         :and :collect gensym :into ignored
                       :finally (return (values names ignored)))
               `(destructuring-bind ,names ,args
                  (declare (ignore . ,ignored))
                  ,@body))))
         (list . ,(mapcar #'cadr bindings)))))))
