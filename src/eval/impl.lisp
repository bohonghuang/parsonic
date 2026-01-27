(in-package #:parsonic)

(defstruct parse-failure
  position expected)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'parser-call) (fdefinition 'funcall)))

(defun parser/satisfies (pred)
  (lambda (input &aux (position (input-position/eval input)) (result (input-read/eval input)))
    (if (funcall pred result)
        result
        (make-parse-failure :position position :expected pred))))

(defun parser/cons (car cdr)
  (lambda (input)
    (let ((car (parser-call car input)))
      (if (parse-failure-p car)
          car
          (let ((cdr (parser-call cdr input)))
            (if (parse-failure-p cdr)
                cdr
                (cons car cdr)))))))

(defun parser/%or (parsers)
  (lambda (input)
    (loop :with position := (input-position/eval input)
          :for parser :in parsers
          :for result := (parser-call parser input)
          :if (parse-failure-p result)
            :collect result :into errors
          :else
            :return result
          :do (setf (input-position/eval input) position)
          :finally (return (make-parse-failure
                            :position (input-position/eval input)
                            :expected (cons 'or (mapcar #'parse-failure-expected errors)))))))

(defun parser/or (&rest parsers)
  (parser/%or parsers))

(defun parser/rep (parser from to)
  (check-type from non-negative-fixnum)
  (check-type to non-negative-fixnum)
  (assert (<= from to))
  (lambda (input)
    (loop :for i :below to
          :for position := (input-position/eval input)
          :for result := (parser-call parser input)
          :if (parse-failure-p result)
            :do (setf (input-position/eval input) position)
            :and
            :if (>= i from)
              :return results
            :else
              :return result
          :else
            :collect result :into results
          :finally (return results))))

(defun parser/constantly (object)
  (constantly object))

(defun parser/apply (function parser)
  (lambda (input &aux (result (parser-call parser input)))
    (if (parse-failure-p result)
        result
        (parser-call (apply function result) input))))

(defun parser/cut (parser)
  (lambda (input &aux (result (parser-call parser input)))
    (if (parse-failure-p result)
        (throw 'parser-run result)
        result)))

(defgeneric expand-expr/eval (op &rest args)
  (:method (op &rest args)
    (apply #'expand-expr op args))
  (:method ((op (eql 'function)) &rest args)
    `(function . ,(expand args)))
  (:method ((op (eql 'lambda)) &rest args)
    (destructuring-bind (lambda-list &rest body) args
      `(lambda ,lambda-list . ,body)))
  (:method ((op (eql 'curry)) &rest args)
    (destructuring-bind (function &rest args) args
      `(curry ,(expand function) . ,(mapcar #'expand args))))
  (:method ((op (eql 'rcurry)) &rest args)
    (destructuring-bind (function &rest args) args
      `(rcurry ,(expand function) . ,(mapcar #'expand args))))
  (:method ((op (eql 'parser-call)) &rest args)
    (destructuring-bind (function &rest args) args
      `(parser-call ,(expand function) . ,(mapcar #'expand args)))))

(defvar *expand*)

(defgeneric expand/eval (object)
  (:method (object) object)
  (:method ((symbol symbol)) symbol)
  (:method ((list list))
    (let ((*expand* #'expand/eval))
      (apply #'expand-expr/eval list))))

(defun parser-run (parser input)
  (let ((result (catch 'parser-run
                  (call-with-input/eval
                   (lambda (input) (parser-call parser input))
                   input))))
    (if (parse-failure-p result)
        (values result (parse-failure-position result))
        result)))
