(in-package #:parsonic)

(defmethod expand-expr :around ((op (eql 'satisfies)) &rest args)
  (destructuring-bind (predicate) args
    (if (and (consp predicate) (equal predicate `(curry #'eql ,(caddr predicate))))
        (call-next-method)
        (call-next-method
         op (with-gensyms (result)
              `(lambda (,result)
                 (unless (eql ,result +input-eof+)
                   (funcall ,predicate ,result))))))))

(defmethod expand-expr :around ((op (eql 'rep)) &rest args)
  (destructuring-bind (parser &optional (from 0) (to most-positive-fixnum)) args
    (call-next-method op parser from to)))
