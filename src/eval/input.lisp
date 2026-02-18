(in-package #:parsonic)

(defgeneric input-read/eval (input))

(defgeneric input-position/eval (input))

(defgeneric (setf input-position/eval) (value input))

(defgeneric call-with-input/eval (thunk input)
  (:method (thunk input) (funcall thunk input)))

(defconstant +input-eof+ 'eof)

(defstruct vector-input
  (vector #() :type vector)
  (index 0 :type non-negative-fixnum))

(defmethod input-read/eval ((input vector-input))
  (let ((vector (vector-input-vector input))
        (index (vector-input-index input)))
    (if (< index (length vector))
        (progn (incf (vector-input-index input)) (aref vector index))
        +input-eof+)))

(defmethod input-position/eval ((input vector-input))
  (vector-input-index input))

(defmethod (setf input-position/eval) (value (input vector-input))
  (setf (vector-input-index input) value))

(defmethod call-with-input/eval (thunk (input vector))
  (funcall thunk (make-vector-input :vector input)))

(defstruct list-input
  (list nil :type list))

(defmethod input-read/eval ((input list-input))
  (let ((list (list-input-list input)))
    (if list
        (car (shiftf (list-input-list input) (cdr list)))
        +input-eof+)))

(defmethod input-position/eval ((input list-input))
  (list-input-list input))

(defmethod (setf input-position/eval) (value (input list-input))
  (setf (list-input-list input) value))

(defmethod call-with-input/eval (thunk (input list))
  (funcall thunk (make-list-input :list input)))

(defmethod input-read/eval ((input stream))
  (handler-case (funcall (eswitch ((stream-element-type input) :test #'equal)
                           ('(unsigned-byte 8) #'read-byte)
                           ('character #'read-char))
                         input)
    (end-of-file () +input-eof+)))

(defmethod input-position/eval ((input stream))
  (file-position input))

(defmethod (setf input-position/eval) (value (input stream))
  (file-position input value))

(defmethod call-with-input/eval (thunk (input stream))
  (funcall thunk input))
