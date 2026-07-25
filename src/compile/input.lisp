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

(defgeneric call-with-enough-input/compile (body input length)
  (:method (body input length) (declare (ignore length)) (funcall body input)))

(defvar *input-type-mappings* nil)

(defvar *input-index*)
(defvar *input-length*)

(defun simple-array-call-with-input/compile (body input type)
  (with-gensyms (index length)
    `(locally (declare (type ,type ,input))
       (let ((,index 0) (,length (length ,input)))
         (declare (type non-negative-fixnum ,index ,length))
         ,(let ((*input-index* index)
                (*input-length* length))
            (funcall body input))))))

(defun simple-array-position/compile (input)
  (declare (ignore input))
  *input-index*)

(defun (setf simple-array-position/compile) (value input)
  (declare (ignore input))
  `(setf ,*input-index* ,value))

(defun simple-array-call-with-enough-input/compile (body input length)
  (if (eql *input-length* most-positive-fixnum)
      (funcall body input)
      `(when (<= (the non-negative-fixnum (+ ,*input-index* ,length)) ,*input-length*)
         ,(let ((*input-length* most-positive-fixnum))
            (funcall body input)))))

(defun simple-array-read/compile (input)
  (let ((index *input-index*)
        (length *input-length*))
    (if (eql length most-positive-fixnum)
        `(prog1 (aref ,input ,index) (incf ,index))
        `(if (< ,index ,length)
             (prog1 (aref ,input ,index) (incf ,index))
             +input-eof+))))

(defconstant +input-type-simple-array-unsigned-byte-8+ (intern (princ-to-string '(simple-array (unsigned-byte 8) (*))) #.*package*))

(defmethod call-with-input/compile (body (input (eql +input-type-simple-array-unsigned-byte-8+)))
  (simple-array-call-with-input/compile body input '(simple-array (unsigned-byte 8) (*))))

(defmethod input-position/compile ((input (eql +input-type-simple-array-unsigned-byte-8+)))
  (simple-array-position/compile input))

(defmethod (setf input-position/compile) (value (input (eql +input-type-simple-array-unsigned-byte-8+)))
  (setf (simple-array-position/compile input) value))

(defmethod input-read/compile ((input (eql +input-type-simple-array-unsigned-byte-8+)))
  (simple-array-read/compile input))

(defmethod call-with-enough-input/compile (body (input (eql +input-type-simple-array-unsigned-byte-8+)) length)
  (simple-array-call-with-enough-input/compile body input length))

(setf (assoc-value *input-type-mappings* '(simple-array (unsigned-byte 8) (*)) :test #'equal) +input-type-simple-array-unsigned-byte-8+)

(defconstant +input-type-simple-array-character+ (intern (princ-to-string '(simple-array character (*))) #.*package*))

(defmethod call-with-input/compile (body (input (eql +input-type-simple-array-character+)))
  (simple-array-call-with-input/compile body input '(simple-array character (*))))

(defmethod input-position/compile ((input (eql +input-type-simple-array-character+)))
  (simple-array-position/compile input))

(defmethod (setf input-position/compile) (value (input (eql +input-type-simple-array-character+)))
  (setf (simple-array-position/compile input) value))

(defmethod input-read/compile ((input (eql +input-type-simple-array-character+)))
  (simple-array-read/compile input))

(defmethod call-with-enough-input/compile (body (input (eql +input-type-simple-array-character+)) length)
  (simple-array-call-with-enough-input/compile body input length))

(setf (assoc-value *input-type-mappings* '(simple-array character (*)) :test #'equal) +input-type-simple-array-character+)

(defun binary-stream-p (stream)
  (subtypep (stream-element-type stream) '(unsigned-byte 8)))

(deftype binary-input-stream ()
  '(and stream (satisfies binary-stream-p) (satisfies input-stream-p)))

(defmethod call-with-input/compile (body (input (eql 'binary-input-stream)))
  (funcall body input))

(defmethod input-position/compile ((input (eql 'binary-input-stream)))
  `(file-position ,input))

(defmethod (setf input-position/compile) (value (input (eql 'binary-input-stream)))
  `(file-position ,input ,value))

(defmethod input-read/compile ((input (eql 'binary-input-stream)))
  `(read-byte ,input nil +input-eof+))

(defun character-stream-p (stream)
  (subtypep (stream-element-type stream) 'character))

(deftype character-input-stream ()
  '(and stream (satisfies character-stream-p) (satisfies input-stream-p)))

(defmethod call-with-input/compile (body (input (eql 'character-input-stream)))
  (funcall body input))

(defmethod input-position/compile ((input (eql 'character-input-stream)))
  `(file-position ,input))

(defmethod (setf input-position/compile) (value (input (eql 'character-input-stream)))
  `(file-position ,input ,value))

(defmethod input-read/compile ((input (eql 'character-input-stream)))
  `(read-char ,input nil +input-eof+))
