(in-package #:parsonic)

(defgeneric input-read/eval (input))

(defgeneric input-position/eval (input))

(defgeneric (setf input-position/eval) (value input))

(defgeneric call-with-input/eval (thunk input)
  (:method (thunk input) (funcall thunk input)))

(defconstant +input-eof+ 'eof)

(defstruct string-input
  (string "" :type (simple-array character (*)))
  (index 0 :type non-negative-fixnum))

(defmethod input-read/eval ((input string-input))
  (let ((string (string-input-string input))
        (index (string-input-index input)))
    (if (< index (length string))
        (progn (incf (string-input-index input)) (aref string index))
        +input-eof+)))

(defmethod input-position/eval ((input string-input))
  (string-input-index input))

(defmethod (setf input-position/eval) (value (input string-input))
  (setf (string-input-index input) value))

(defmethod call-with-input/eval (thunk (input array))
  (etypecase input
    ((array character) (funcall thunk (make-string-input :string (coerce input '(simple-array character (*))))))))

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
