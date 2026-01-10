(in-package #:parseq)

(defmethod expand-quote ((vector vector))
  (expand
   `(progn
      ,@(map 'list (curry #'list 'eql) vector)
      (constantly ,vector))))

(defmethod expand-quote ((character character))
  (expand `(eql ,character)))
