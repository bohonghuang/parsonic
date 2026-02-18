(in-package #:parsonic)

(defun compute-bounds (form)
  (destructuring-ecase form
    ((parser/satisfies &rest args)
     (declare (ignore args))
     (values 1 1))
    ((parser/constantly &rest args)
     (declare (ignore args))
     (values 0 0))
    ((parser/cons &rest args)
     (destructuring-bind (car cdr) args
       (multiple-value-bind (min1 max1) (compute-bounds car)
         (multiple-value-bind (min2 max2) (compute-bounds cdr)
           (values (+ min1 min2) (+ max1 max2))))))
    ((parser/or &rest args)
     (loop :for arg :in (or args (return (values 0 most-positive-fixnum)))
           :for (min-arg max-arg) := (multiple-value-list (compute-bounds arg))
           :minimize min-arg :into min :of-type non-negative-integer
           :maximize max-arg :into max :of-type non-negative-integer
           :finally (return (values min max))))
    ((parser/rep &rest args)
     (destructuring-bind (parser from to) args
       (multiple-value-bind (min-parser max-parser) (compute-bounds parser)
         (values (if (integerp from) (* from min-parser) 0)
                 (if (integerp to) (* to max-parser) most-positive-fixnum)))))
    ((parser/apply &rest args)
     (destructuring-bind (function parser) args
       (multiple-value-bind (min-args max-args) (compute-bounds parser)
         (let ((min-fn nil) (max-fn nil))
           (walk-parsers-in-lambda
            (lambda (parser)
              (multiple-value-bind (min-parser max-parser) (compute-bounds parser)
                (if min-fn (minf min-fn min-parser) (setf min-fn min-parser))
                (if max-fn (maxf max-fn max-parser) (setf max-fn max-parser))))
            function)
           (values (+ min-args (or min-fn 0)) (+ max-args (or max-fn most-positive-fixnum)))))))
    ((parser/call &rest args)
     (declare (ignore args))
     (values 0 most-positive-fixnum))
    (((parser/let parser/unit parser/cut) &rest args)
     (compute-bounds (lastcar args)))))
