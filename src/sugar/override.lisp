(in-package #:parsonic)

(declaim (inline not-eof-p))
(defun not-eof-p (object)
  (not (eq object +input-eof+)))

(defmethod expand-expr :around ((op symbol) &rest args)
  (destructuring-case (cons op args)
    ((satisfies predicate)
     (if (and (consp predicate) (equal predicate `(curry #'eql ,(caddr predicate))))
         (call-next-method)
         (call-next-method
          op (if (equal predicate '(constantly t))
                 '#'not-eof-p
                 (with-gensyms (result)
                   `(lambda (,result)
                      (and (not-eof-p ,result) (funcall ,predicate ,result))))))))
    ((rep parser &optional (from 0) (to most-positive-fixnum))
     (call-next-method op parser from to))
    ((t &rest args) (declare (ignore args)) (call-next-method))))
