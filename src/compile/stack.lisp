(in-package #:parsonic)

(defvar *codegen-cons*)
(defvar *codegen-blocks*)
(defvar *codegen-list-vars*)

(defparameter *emulate-stack-allocation-p* '(#-(or sbcl ccl) t))

(defun codegen-make-list (&optional (dimensions nil dimensionsp))
  (if dimensionsp
      (with-gensyms (list)
        (let* ((dimensions (ensure-list dimensions))
               (dimensions (if *emulate-stack-allocation-p*
                               (loop :for size :in dimensions :collect (- (abs size)))
                               dimensions)))
          (push (cons list dimensions) *codegen-list-vars*))
        list)
      #'codegen-make-list))

(defparameter *codegen-make-list* #'codegen-make-list)

(defun call-with-fresh-stack (body)
  (with-gensyms (result error-info)
    (let ((*codegen-list-vars* nil))
      (let ((body (funcall body)))
        (if *codegen-list-vars*
            `(let ,(loop :for (var . (size)) :in *codegen-list-vars*
                         :if (plusp size)
                           :collect `(,var (make-list ,size))
                         :else :if (minusp size)
                           :collect `(,var ,(loop :for form := nil :then (funcall *codegen-cons* nil form)
                                                  :repeat (- size) :finally (return form)))
                         :else
                           :collect `(,var nil))
               (declare (dynamic-extent . ,(loop :for (var . (size)) :in *codegen-list-vars* :when (plusp size) :collect var)))
               (multiple-value-bind (,result ,error-info) (block ,(car *codegen-blocks*) ,body)
                 ,@(loop :for (var . dimensions) :in *codegen-list-vars*
                         :collect (labels ((recur (var dimensions &aux (size (car dimensions)))
                                             (with-gensyms (cons elem rest)
                                               (if (and size (not (plusp size)))
                                                   `(when ,var
                                                      (loop :for ,cons :on ,var
                                                            :for (,elem . ,rest) := ,cons
                                                            :do ,(recur elem (cdr dimensions))
                                                            :while ,rest
                                                            ,@(unless (zerop size) `(:repeat ,(1- (- size))))
                                                            :finally (setf (cdr ,cons) ,(funcall *codegen-cons* var))))
                                                   `(progn ,var)))))
                                    (recur var dimensions)))
                 (when ,(codegen-parse-error-p result)
                   (return-from ,(car *codegen-blocks*) (values ,result ,error-info)))
                 ,result))
            body)))))

(defmacro with-fresh-stack (&body body)
  `(call-with-fresh-stack (lambda () . ,body)))

(defun call-with-nested-stack (body)
  (with-gensyms (result error-info)
    (multiple-value-bind (body list-vars)
        (let ((*codegen-list-vars* nil)
              (*emulate-stack-allocation-p* t))
          (values (funcall body) *codegen-list-vars*))
      (if list-vars
          (let ((lists (loop :for (var . dimensions) :in list-vars
                             :collect (cons var (funcall *codegen-make-list* (cons 0 dimensions))))))
            `(let ,(loop :for (var . (size)) :in list-vars
                         :do (check-type size non-positive-fixnum)
                         :collect `(,var ,(loop :for form := nil :then (funcall *codegen-cons* nil form)
                                                :repeat (- size)
                                                :finally (return form))))
               (multiple-value-bind (,result ,error-info) (block ,(car *codegen-blocks*) ,body)
                 ,@(loop :for (var . list) :in lists
                         :collect `(setf ,list ,(funcall *codegen-cons* var list)))
                 (when ,(codegen-parse-error-p result)
                   (return-from ,(car *codegen-blocks*) (values ,result ,error-info)))
                 ,result)))
          body))))

(defmacro with-nested-stack (&body body)
  `(call-with-nested-stack (lambda () . ,body)))
