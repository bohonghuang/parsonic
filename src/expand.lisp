(in-package #:parsonic)

(defun parser-name-symbol (name &optional (intern t))
  (let ((package (if (eq (symbol-package name) #.(find-package :cl)) #.(find-package '#:parsonic) (symbol-package name)))
        (name (format nil "~A/~A" (string '#:parser) name)))
    (if intern (intern name package) (find-symbol name package))))

(declaim (type function *expand*))
(defvar *expand* #'expand/eval)

(defun expand (object)
  (funcall *expand* object))

(defmacro parser (form)
  (expand form))

(defgeneric expand-quote (object)
  (:method (object) `(quote ,object)))

(defgeneric expand-expr (op &rest args)
  (:method ((symbol symbol) &rest args)
    `(,(or (parser-name-symbol symbol nil)
           (if (fboundp symbol)
               (return-from expand-expr (cons symbol args))
               (parser-name-symbol symbol)))
      . ,(mapcar #'expand args)))
  (:method ((op (eql 'quote)) &rest args)
    (destructuring-bind (object) args
      (expand-quote object)))
  (:method ((op (eql 'satisfies)) &rest args)
    (destructuring-bind (predicate) args
      `(parser/satisfies ,predicate)))
  (:method ((op (eql 'cons)) &rest args)
    (destructuring-bind (car cdr) args
      `(parser/cons ,(expand car) ,(expand cdr))))
  (:method ((op (eql 'or)) &rest args)
    `(parser/or . ,(mapcar #'expand args)))
  (:method ((op (eql 'constantly)) &rest args)
    (destructuring-bind (object) args
      `(parser/constantly ,object)))
  (:method ((op (eql 'rep)) &rest args)
    (destructuring-bind (parser from to) args
      `(parser/rep ,(expand parser) ,from ,to)))
  (:method ((op (eql 'apply)) &rest args)
    (destructuring-bind (function parser) args
      `(parser/apply ,function ,(expand parser)))))
