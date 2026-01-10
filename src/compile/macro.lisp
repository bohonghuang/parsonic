(in-package #:parseq)

(defun body-declarations (body)
  (values
   (loop :for (declaration . rest) :on body
         :while (and (listp declaration) (eq (car declaration) 'declare))
         :do (setf body rest)
         :append (cdr declaration))
   body))

(defparameter *flatten-local-functions-p* nil)

(defmacro parser-lambda ((input-var) &body body)
  (multiple-value-bind (declarations body) (body-declarations body)
    (let ((types (mapcar #'cdr (remove 'type declarations :key #'car :test-not #'eq))))
      (let ((input (if types
                       (destructuring-bind ((input input-var-2)) types
                         (assert (eq input-var input-var-2))
                         (or (assoc-value *input-type-mappings* input :test #'type=) input))
                       input-var)))
        (with-gensyms (block cons-pool cons car cdr cons-alloc cons-free)
          `(lambda (,input-var &aux (,(intern (princ-to-string input)) ,input-var))
             (declare . ,declarations)
             (let ((,cons-pool nil))
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
                        (when ,cons
                          (loop :for ,cons-free :on ,cons
                                :unless (cdr ,cons-free)
                                  :return (setf (cdr ,cons-free) ,cons-pool
                                                ,cons-pool ,cons)))))
                 (declare (ignorable #',cons-alloc #',cons-free) (inline ,cons-alloc ,cons-free))
                 ,(call-with-input/compile
                   (lambda (input)
                     (let ((*codegen-input* input)
                           (*codegen-blocks* (list block))
                           (*codegen-cons* (lambda (car &optional (cdr nil cdrp))
                                             (if cdrp
                                                 `(,cons-alloc ,car ,cdr)
                                                 `(,cons-free (shiftf ,car nil)))))
                           (*codegen-make-list* (lambda (size)
                                                  (with-gensyms (list)
                                                    (push (cons list size) *codegen-list-vars*)
                                                    list)))
                           (*codegen-labels* (if *flatten-local-functions-p*
                                                 (let ((local-functions nil))
                                                   (lambda (functions-or-body &optional (body nil bodyp))
                                                     (if bodyp
                                                         (progn (nconcf local-functions functions-or-body) body)
                                                         `(labels ,local-functions ,functions-or-body))))
                                                 (lambda (functions-or-body &optional (body nil bodyp))
                                                   (if bodyp
                                                       `(labels ,functions-or-body ,body)
                                                       functions-or-body)))))
                       `(block ,block ,(funcall *codegen-labels* (with-fresh-stack (codegen (codegen-expand `(progn . ,body))))))))
                   input)))))))))
