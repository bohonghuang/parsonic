(in-package #:parseq)

(defun body-declarations (body)
  (values
   (loop :for (declaration . rest) :on body
         :while (and (listp declaration) (eq (car declaration) 'declare))
         :do (setf body rest)
         :append (cdr declaration))
   body))

(defparameter *emulate-stack-allocation-p* '(#-(or sbcl ccl) t))
(defparameter *flatten-local-functions-p* nil)

(defmacro parser-lambda ((input-var) &body body)
  (multiple-value-bind (declarations body) (body-declarations body)
    (let ((types (mapcar #'cdr (remove 'type declarations :key #'car :test-not #'eq))))
      (let ((input (if types
                       (destructuring-bind ((input input-var-2)) types
                         (assert (eq input-var input-var-2))
                         (or (assoc-value *input-type-mappings* input :test #'type=) input))
                       input-var)))
        (with-gensyms (block)
          `(lambda (,input-var &aux (,(intern (princ-to-string input)) ,input-var))
             (declare . ,declarations)
             ,(call-with-cons-pool/compile
               (lambda (cons-alloc cons-free)
                 (call-with-input/compile
                  (lambda (input)
                    (let ((*codegen-input* input)
                          (*codegen-blocks* (list block))
                          (*codegen-cons* (lambda (car &optional (cdr nil cdrp))
                                            (if cdrp
                                                `(,cons-alloc ,car ,cdr)
                                                (if (and (listp car) (eq (first car) 'subseq))
                                                    `(,cons-free (shiftf ,(second car) nil) ,(third car) ,(fourth car))
                                                    `(,cons-free (shiftf ,car nil))))))
                          (*codegen-make-list* (lambda (size)
                                                 (with-gensyms (list)
                                                   (when *emulate-stack-allocation-p*
                                                     (setf size (- size)))
                                                   (push (cons list size) *codegen-list-vars*)
                                                   list)))
                          (*codegen-labels* (let ((local-functions nil))
                                              (lambda (functions-or-body &optional (body nil bodyp))
                                                (if *flatten-local-functions-p*
                                                    (if bodyp
                                                        (progn (nconcf local-functions functions-or-body) body)
                                                        `(labels ,local-functions ,functions-or-body))
                                                    (if bodyp
                                                        `(labels ,functions-or-body ,body)
                                                        functions-or-body))))))
                      (multiple-value-bind (form functions) (extract/compile (codegen-expand `(progn . ,body)))
                        (let ((body `(block ,block ,(funcall *codegen-labels* (with-fresh-stack (codegen form))))))
                          (funcall
                           *codegen-labels*
                           (let ((*flatten-local-functions-p* nil))
                             (funcall
                              *codegen-labels*
                              (loop :for (name lambda-list parser) :in functions
                                    :collect (list name lambda-list (let ((*codegen-blocks* (list name))) (with-fresh-stack (codegen parser)))))
                              body)))))))
                  input)))))))))
