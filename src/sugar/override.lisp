(in-package #:parsonic)

(declaim (inline not-eof-p))
(defun not-eof-p (object)
  (not (eq object +input-eof+)))

(defmethod expand-expr ((op (eql 'satisfies)) &rest args)
  (destructuring-bind (predicate) args
    `(parser/satisfies
      ,(cond
         ((and (consp predicate) (equal predicate `(curry #'eql ,(caddr predicate)))) predicate)
         ((equal predicate '(constantly t)) '#'not-eof-p)
         (t (with-gensyms (result)
              `(lambda (,result)
                 (and (not-eof-p ,result) (funcall ,predicate ,result)))))))))

(defmethod expand-expr ((op (eql 'rep)) &rest args)
  (destructuring-bind (parser &optional (from 0) (to most-positive-fixnum)) args
    `(parser/rep ,(expand parser) ,from ,to)))
