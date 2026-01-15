(in-package #:parsonic)

(defmethod expand-expr ((op (eql 'funcall)) &rest args)
  (destructuring-bind (function &rest args) args
    (expand `(apply ,function (list . ,args)))))

(defmethod expand-expr ((op (eql 'list)) &rest args)
  (loop :for body := '(parser/constantly nil) :then `(parser/cons ,(expand arg) ,body)
        :for arg :in (reverse args)
        :finally (return body)))

(defmethod expand-expr :around ((op (eql 'or)) &rest args)
  (case (length args)
    (0 (expand '(satisfies (constantly nil))))
    (1 (expand (first args)))
    (t (apply #'call-next-method (reduce (curry #'list 'or) args :from-end t)))))

(defmethod expand-expr ((op (eql 'progn)) &rest args)
  (with-gensyms (var)
    (expand `(for ((nil (list . ,(butlast args)))
                   (,var ,(lastcar args)))
               ,var))))

(defmethod expand-expr ((op (eql 'prog1)) &rest args)
  (with-gensyms (var)
    (expand `(for ((,var ,(first args))
                   (nil (list . ,(rest args))))
               ,var))))

(defmethod expand-expr ((op (eql 'prog2)) &rest args)
  (with-gensyms (var)
    (expand `(for ((nil ,(first args))
                   (,var ,(second args))
                   (nil (list . ,(nthcdr 2 args))))
               ,var))))
