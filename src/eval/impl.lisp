(in-package #:parsonic)

(progn
  (defun parser-error-p (err)
    (when (consp err)
      (eq (car err) #1='#:error)))
  (defun parser-error (&rest args)
    (cons #1# args)))

(defun parser-error-expected (err)
  (getf (cdr err) :expected))

(defun (setf parser-error-expected) (value err)
  (setf (getf (cdr err) :expected) value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'parser-call) (fdefinition 'funcall)))

(defun parser/satisfies (pred)
  (lambda (input &aux (position (input-position/eval input)) (result (input-read/eval input)))
    (if (funcall pred result)
        result
        (parser-error :position position :expected pred))))

(defun parser/cons (car cdr)
  (lambda (input)
    (let ((car (parser-call car input)))
      (if (parser-error-p car)
          car
          (let ((cdr (parser-call cdr input)))
            (if (parser-error-p cdr)
                cdr
                (cons car cdr)))))))

(defun parser/%or (parsers)
  (lambda (input)
    (loop :with position := (input-position/eval input)
          :for parser :in parsers
          :for result := (parser-call parser input)
          :if (parser-error-p result)
            :collect result :into errors
          :else
            :return result
          :do (setf (input-position/eval input) position)
          :finally (return (parser-error :position (input-position/eval input) :expected (cons 'or (mapcar #'cdr errors)))))))

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
          :if (parser-error-p result)
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
    (if (parser-error-p result)
        result
        (parser-call (apply function result) input))))

(defun parser/cut (parser)
  (lambda (input &aux (result (parser-call parser input)))
    (if (parser-error-p result)
        (throw 'parser-run result)
        result)))

(defgeneric expand/eval (object)
  (:method ((symbol symbol)) symbol)
  (:method ((integer integer)) integer)
  (:method ((list list)) (apply #'expand-expr list)))

(defun parser-run (parser input)
  (catch 'parser-run
    (call-with-input/eval
     (lambda (input) (parser-call parser input))
     input)))
