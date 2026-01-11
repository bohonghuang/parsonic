(in-package #:parsonic)

(defun call-with-cons-pool/compile (thunk)
  (with-gensyms (cons-pool cons car cdr cons-alloc cons-free from to cons-free-macro)
    `(let ((,cons-pool nil))
       (declare (ignorable ,cons-pool))
       (flet ((,cons-alloc (,car ,cdr)
                (let ((,cons ,cons-pool))
                  (if ,cons
                      (progn
                        (setf ,cons-pool (cdr ,cons-pool)
                              (car ,cons) ,car
                              (cdr ,cons) ,cdr)
                        ,cons)
                      (cons ,car ,cdr))))
              (,cons-free (,cons)
                (loop :for ,cons-free :on ,cons
                      :unless (cdr ,cons-free)
                        :return (setf (cdr ,cons-free) ,cons-pool ,cons-pool ,cons))))
         (declare (ignorable #',cons-alloc #',cons-free) (inline ,cons-alloc ,cons-free))
         (macrolet ((,cons-free-macro (,cons &optional ,from ,to)
                      (if ,from
                          (progn
                            (check-type ,from (eql 0))
                            (check-type ,to positive-fixnum)
                            (once-only (,cons)
                              `(setf (cdr (nthcdr ,(1- ,to) ,,cons)) ,',cons-pool ,',cons-pool ,,cons)))
                          `(,',cons-free ,,cons))))
           ,(funcall thunk cons-alloc cons-free-macro))))))
