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
    (destructuring-bind (object) args
      (with-gensyms (result)
        `(parser/satisfies
          (lambda (,result)
            (unless (eql ,result +input-eof+)
              (funcall ,object ,result)))))))
  (:method ((op (eql 'eql)) &rest args)
    (destructuring-bind (object) args
      `(parser/eql ,object)))
  (:method ((op (eql 'cons)) &rest args)
    (destructuring-bind (car cdr) args
      `(parser/cons ,(expand car) ,(expand cdr))))
  (:method ((op (eql 'or)) &rest args)
    `(parser/or . ,(mapcar #'expand args)))
  (:method ((op (eql 'constantly)) &rest args)
    (destructuring-bind (object) args
      `(parser/constantly ,object)))
  (:method ((op (eql 'rep)) &rest args)
    (destructuring-bind (parser &optional from to) args
      (declare (ignore parser from to))
      `(parser/rep ,(expand (car args)) . ,(cdr args))))
  (:method ((op (eql 'funcall)) &rest args)
    (destructuring-bind (function &rest parsers) args
      `(parser/funcall ,function . ,(mapcar #'expand parsers)))))
