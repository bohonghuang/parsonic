(in-package #:parsonic)

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
               (lambda ()
                 (call-with-input/compile
                  (lambda (input)
                    (let ((local-functions nil))
                      (let ((*codegen-input* input)
                            (*codegen-blocks* (list block))
                            (*codegen-labels* (lambda (functions-or-body body) (nconcf local-functions functions-or-body) body)))
                        (multiple-value-bind (form functions) (extract/compile (codegen-expand `(progn . ,body)))
                          (let ((body (with-fresh-stack (codegen form))))
                            `(block ,block
                               (labels ,(append
                                         (loop :for (name lambda-list parser) :in functions
                                               :collect (list name lambda-list
                                                              (let ((*codegen-blocks* (cons name *codegen-blocks*)))
                                                                (with-fresh-stack (codegen parser)))))
                                         local-functions)
                                 ,body)))))))
                  input)))))))))
