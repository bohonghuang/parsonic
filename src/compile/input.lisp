(in-package #:parsonic)

(defconstant +input-eof+ 'eof)

(defgeneric input-read/compile (input)
  (:method (input) `(input-read/eval ,input)))

(defgeneric input-position/compile (input)
  (:method (input) `(input-position/eval ,input)))

(defgeneric (setf input-position/compile) (value input)
  (:method (value input) `(setf (input-position/eval ,input) ,value)))

(defgeneric call-with-input/compile (body input)
  (:method (body input) `(call-with-input/eval (lambda (,input) ,(funcall body input)) ,input)))

(defvar *input-type-mappings* nil)

(defvar *input-index*)
(defvar *input-length*)

(defconstant +input-type-simple-array-character+ (intern (princ-to-string '(simple-array character (*)))))

(defmethod call-with-input/compile (body (input (eql +input-type-simple-array-character+)))
  (with-gensyms (index length)
    (let ((input-var (intern (princ-to-string input))))
      `(locally (declare (type (simple-array character (*)) ,input-var))
         (let ((,index 0) (,length (length ,input-var)))
           (declare (type non-negative-fixnum ,index ,length))
           ,(let ((*input-index* index)
                  (*input-length* length))
              (funcall body input)))))))

(defmethod input-position/compile ((input (eql +input-type-simple-array-character+)))
  *input-index*)

(defmethod (setf input-position/compile) (value (input (eql +input-type-simple-array-character+)))
  `(setf ,*input-index* ,value))

(defmethod input-read/compile ((input (eql +input-type-simple-array-character+)))
  (let ((index *input-index*)
        (length *input-length*))
    `(if (< ,index ,length)
         (prog1 (aref ,(intern (princ-to-string input)) ,index) (incf ,index))
         +input-eof+)))

(setf (assoc-value *input-type-mappings* '(simple-array character (*)) :test #'equal) +input-type-simple-array-character+)
