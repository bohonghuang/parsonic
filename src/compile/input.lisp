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

(defconstant +input-type-simple-array-unsigned-byte-8+ (intern (princ-to-string '(simple-array (unsigned-byte 8) (*))) #.*package*))

(defmethod call-with-input/compile (body (input (eql +input-type-simple-array-unsigned-byte-8+)))
  (with-gensyms (index length)
    `(locally (declare (type (simple-array (unsigned-byte 8) (*)) ,input))
       (let ((,index 0) (,length (length ,input)))
         (declare (type non-negative-fixnum ,index ,length))
         ,(let ((*input-index* index)
                (*input-length* length))
            (funcall body input))))))

(defmethod input-position/compile ((input (eql +input-type-simple-array-unsigned-byte-8+)))
  *input-index*)

(defmethod (setf input-position/compile) (value (input (eql +input-type-simple-array-unsigned-byte-8+)))
  `(setf ,*input-index* ,value))

(defmethod input-read/compile ((input (eql +input-type-simple-array-unsigned-byte-8+)))
  (let ((index *input-index*)
        (length *input-length*))
    `(if (< ,index ,length)
         (prog1 (aref ,input ,index) (incf ,index))
         +input-eof+)))

(setf (assoc-value *input-type-mappings* '(simple-array (unsigned-byte 8) (*)) :test #'equal) +input-type-simple-array-unsigned-byte-8+)

(defconstant +input-type-simple-array-character+ (intern (princ-to-string '(simple-array character (*))) #.*package*))

(defmethod call-with-input/compile (body (input (eql +input-type-simple-array-character+)))
  (with-gensyms (index length)
    `(locally (declare (type (simple-array character (*)) ,input))
       (let ((,index 0) (,length (length ,input)))
         (declare (type non-negative-fixnum ,index ,length))
         ,(let ((*input-index* index)
                (*input-length* length))
            (funcall body input))))))

(defmethod input-position/compile ((input (eql +input-type-simple-array-character+)))
  *input-index*)

(defmethod (setf input-position/compile) (value (input (eql +input-type-simple-array-character+)))
  `(setf ,*input-index* ,value))

(defmethod input-read/compile ((input (eql +input-type-simple-array-character+)))
  (let ((index *input-index*)
        (length *input-length*))
    `(if (< ,index ,length)
         (prog1 (aref ,input ,index) (incf ,index))
         +input-eof+)))

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
