(in-package #:parseq)

(defun parser-name-symbol (name)
  (intern
   (format nil "~A/~A" (string '#:parser) name)
   (let ((package (symbol-package name)))
     (if (eq package #.(find-package :cl))
         (find-package '#:parseq)
         package))))

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
    `(,(if (fboundp symbol) symbol (parser-name-symbol symbol)) . ,(mapcar #'expand args)))
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
