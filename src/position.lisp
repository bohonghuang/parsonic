(in-package #:parsonic)

(defmethod expand-expr/eval ((op (eql 'position)) &rest args)
  (destructuring-bind (&optional position) args
    (if position
        `(curry #'(setf input-position/eval) ,position)
        '#'input-position/eval)))

(defmethod expand-expr/compile ((op (eql 'position)) &rest args)
  (destructuring-bind (&optional position) args
    (expand `(notinline (constantly ,(if position
                                         (setf (input-position/compile *codegen-input*) position)
                                         (input-position/compile *codegen-input*)))))))
