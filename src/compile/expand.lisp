(in-package #:parseq)

(defgeneric expand-expr/compile (op &rest args)
  (:method (op &rest args)
    (apply #'expand-expr op args)))

(defvar *expand/compile-variables* nil)
(defvar *expand/compile-known* nil)
(defvar *expand/compile-recursive* nil)

(defgeneric expand/compile (form)
  (:method ((expr list))
    (let ((*expand* #'expand/compile))
      (apply #'expand-expr/compile expr)))
  (:method ((symbol symbol))
    (assert (not (keywordp symbol)))
    (if-let ((cons (assoc symbol *expand/compile-variables*)))
      (expand/compile (cdr cons))
      symbol)))

(defun %expand-expr/compile (name lambda-list args body)
  (let ((parser-args nil))
    (when args
      (let ((*expand* (lambda (object)
                        (typecase object
                          (symbol (push object parser-args))
                          (list
                           (unless (member (car object) *expand/compile-recursive*)
                             (apply #'expand-expr/compile object)))
                          (t nil))))
            (*expand/compile-recursive* (cons name *expand/compile-recursive*)))
        (expand body)))
    (unless *expand/compile-recursive*
      (multiple-value-bind (parsers variables)
          (loop :for (variable . value) :in args
                :if (member variable parser-args)
                  :collect (cons variable (typecase value
                                            (symbol (or (assoc-value *expand/compile-variables* value) (assert nil)))
                                            (t value)))
                    :into parsers
                :else
                  :collect (cons variable value) :into variables
                :finally (return (values parsers variables)))
        (if-let ((fdef (assoc (cons name parsers) *expand/compile-known* :test #'equal)))
          (destructuring-bind (fname) (setf (cdr fdef) (ensure-list (cdr fdef)))
            `(parser/call ,fname . ,(mapcar #'cdr variables)))
          (let* ((fname (gensym (string name)))
                 (lambda-list (remove-if (rcurry #'assoc parsers) lambda-list))
                 (variables (mapcar (lambda (cons) (list (car cons) (cdr cons))) variables))
                 (lambda-list (if (intersection lambda-list lambda-list-keywords)
                                  (append lambda-list '(&initial) variables)
                                  (progn
                                    (assert (equal lambda-list (mapcar #'car variables)))
                                    variables))))
            (let ((*expand/compile-variables* (append parsers *expand/compile-variables*))
                  (*expand/compile-known* (cons (cons (cons name parsers) fname) *expand/compile-known*)))
              (let ((result (expand/compile body)))
                (if (listp (cdr (first *expand/compile-known*)))
                    `(parser/let ,fname ,lambda-list ,result)
                    (if lambda-list
                        `(parser/let nil ,lambda-list ,result)
                        result))))))))))
