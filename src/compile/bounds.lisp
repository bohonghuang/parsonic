(in-package #:parsonic)

(defvar *compute-bounds-env* nil)

(defun compute-bounds-env-ref (value)
  (if-let ((binding (assoc value *compute-bounds-env*)))
    (second binding)
    value))

(defun compute-bounds-env-next (bindings)
  (nconc
   (loop :for (var val) :in bindings
         :collect (list var (compute-bounds-env-ref val)))
   *compute-bounds-env*))

(defun compute-bounds-1/compile (form)
  (destructuring-case form
    ((parser/eql &rest args)
     (declare (ignore args))
     (values 1 1))
    ((parser/eql* object)
     (let ((length (length object)))
       (values length length)))
    ((parser/list &rest args)
     (declare (ignore args))
     (compute-bounds (list->conses-1 form)))
    ((parser/case &rest branches)
     (loop :for (key parser) :in branches
           :for (min-parser max-parser) := (multiple-value-list (compute-bounds parser))
           :minimize min-parser :into min :of-type non-negative-integer
           :maximize max-parser :into max :of-type non-negative-integer
           :finally (return (values (1+ min) (1+ max)))))
    ((parser/funcall function &rest args)
     (compute-bounds `(parser/apply ,function (parser/list . ,args))))
    ((parser/filter function &rest args)
     (declare (ignore function))
     (compute-bounds
      `(parser/funcall
        (lambda ,(loop :for arg :in args :collect (gensym))
          (with-codegen (parser/constantly nil)))
        . ,args)))
    ((parser/let bindings body)
     (let ((*compute-bounds-env* (compute-bounds-env-next bindings)))
       (compute-bounds body)))
    ((parser/apply (lambda lambda-list &rest body) &rest args)
     (assert (eq lambda 'lambda))
     (let ((*compute-bounds-env* (compute-bounds-env-next (mapcar (rcurry #'list '#:unknown) (lambda-list-arguments lambda-list)))))
       (compute-bounds-1/eval `(parser/apply (lambda ,lambda-list . ,body) . ,args))))
    ((parser/rep parser min max)
     (compute-bounds-1/eval `(parser/rep ,parser ,(compute-bounds-env-ref min) ,(compute-bounds-env-ref max))))
    ((parser/unit &rest args)
     (compute-bounds (lastcar args)))
    ((t &rest args) (declare (ignore args)) (compute-bounds-1/eval form))))

(defun compute-bounds/compile (form)
  (let ((*compute-bounds* #'compute-bounds-1/compile))
    (compute-bounds-1/compile form)))
