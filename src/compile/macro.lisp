(in-package #:parsonic)

(defmacro parser/compile (form input)
  (with-gensyms (arg block)
    (let ((input (or (assoc-value *input-type-mappings* input :test #'type=) input)))
      `(lambda (,arg &aux (,(intern (princ-to-string input) #.*package*) ,arg))
         ,(call-with-cons-pool/compile
           (lambda ()
             (call-with-input/compile
              (lambda (input)
                (let ((local-functions nil))
                  (let ((*codegen-input* input)
                        (*codegen-blocks* (list block))
                        (*codegen-labels* (lambda (functions-or-body body) (nconcf local-functions functions-or-body) body)))
                    (multiple-value-bind (form functions) (extract/compile (optimize/compile (expand/compile form)))
                      (let ((body (with-fresh-stack (codegen form))))
                        `(block ,block
                           (labels ,(delete-duplicates
                                     (append
                                         (loop :for (name lambda-list parser) :in functions
                                               :collect (list name lambda-list
                                                              (let ((*codegen-blocks* (cons name *codegen-blocks*)))
                                                                (with-fresh-stack (codegen parser)))))
                                       local-functions)
                                     :key #'first)
                             ,body)))))))
              input)))))))

(define-compiler-macro parser-run (&whole whole parser input)
  (typecase input
    (list
     (destructuring-case input
       ((the type input)
        (typecase parser
          (list
           #+ccl
           (when (and (parser-tree-p parser) (notany #'parser-tree-p (cdr parser)))
             (when-let ((name (parser-symbol-name (car parser))))
               (return-from parser-run `(funcall (parser/compile (,name . ,(cdr parser)) ,type) ,input))))
           (destructuring-case parser
             ((parser form) `(funcall (parser/compile ,form ,type) ,input))
             ((t &rest args) (declare (ignore args)) whole)))
          (t whole)))
       ((t &rest args) (declare (ignore args)) whole)))
    (t whole)))
