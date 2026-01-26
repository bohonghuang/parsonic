(in-package #:parsonic)

(defparameter *emulate-stack-allocation-p* '(#-(or sbcl ccl) t))

(defun call-with-cons-pool/compile (thunk)
  (with-gensyms (cons-pool cons car cdr list cons-next cons-alloc cons-free copy-list)
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
                (prog1 ,cons-pool (setf ,cons-pool ,cons)))
              (,copy-list (,list)
                (loop :for (,car . ,cdr) :on (or ,list (return))
                      :for ,cons := (or ,cons-pool (return (copy-list ,list))) :then ,cons-next
                      :for ,cons-next := (cdr ,cons)
                      :do (setf (car ,cons) ,car)
                      :while ,cons-next
                      :finally
                         (setf (cdr ,cons) (copy-list ,cdr))
                         (return (shiftf ,cons-pool ,cons-next)))))
         (declare (ignorable #',cons-alloc #',cons-free #',copy-list) (inline ,cons-alloc ,cons-free))
         ,(labels ((codegen-cons (&optional (car nil carp) (cdr nil cdrp))
                     (if carp
                         (if cdrp
                             `(,cons-alloc ,car ,cdr)
                             `(,cons-free ,car))
                         #'codegen-cons))
                   (codegen-make-list (&optional (dimensions nil dimensionsp))
                     (if dimensionsp
                         (with-gensyms (list)
                           (when *emulate-stack-allocation-p*
                             (setf dimensions (loop :for size :in (ensure-list dimensions) :collect (- (abs size)))))
                           (push (cons list dimensions) *codegen-list-vars*)
                           list)
                         #'codegen-make-list))
                   (codegen-copy-list (&optional (list nil listp))
                     (if listp `(,copy-list ,list) #'codegen-copy-list)))
            (let ((*codegen-cons* #'codegen-cons)
                  (*codegen-make-list* #'codegen-make-list)
                  (*codegen-copy-list* #'codegen-copy-list))
              (funcall thunk)))))))
