(in-package #:parsonic)

(defmethod expand-expr/eval ((op (eql 'position)) &rest args)
  (destructuring-bind () args
    #'input-position/eval))

(defmethod expand-expr/compile ((op (eql 'position)) &rest args)
  (destructuring-bind () args
    (expand `(notinline (constantly ,(input-position/compile *codegen-input*))))))
