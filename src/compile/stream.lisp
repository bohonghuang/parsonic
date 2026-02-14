(in-package #:parsonic)

(defvar *input-buffer*)
(defvar *input-ring-buffer*)

(defparameter *input-buffer-size* 4096)

(deftype binary-input-stream ()
  'stream)

(defmethod call-with-input/compile (body (input (eql 'binary-input-stream)))
  (with-gensyms (buffer ring-buffer length)
    `(locally (declare (type stream ,input))
       (let* ((,buffer (make-array ,*input-buffer-size* :element-type '(unsigned-byte 8)))
              (,length (length ,buffer))
              (,ring-buffer (buffered-streams::make-stream-ring-buffer :stream ,input :buffer ,buffer)))
         (declare (dynamic-extent ,buffer ,ring-buffer))
         ,(let ((*input-ring-buffer* ring-buffer)
                (*input-buffer* buffer)
                (*input-length* length))
            (funcall body input))))))

(defmethod input-position/compile ((input (eql 'binary-input-stream)))
  `(buffered-streams::stream-ring-buffer-stream-position ,*input-ring-buffer*))

(defmethod (setf input-position/compile) (value (input (eql 'binary-input-stream)))
  `(buffered-streams::stream-ring-buffer-seek ,*input-ring-buffer* ,value))

(defmethod input-read/compile ((input (eql 'binary-input-stream)))
  (let ((buffer *input-buffer*)
        (buffer-length *input-length*)
        (ring-buffer *input-ring-buffer*))
    (with-gensyms (position offset result)
      `(let ((,position (buffered-streams::stream-ring-buffer-stream-position ,ring-buffer))
             (,offset (buffered-streams::stream-ring-buffer-buffer-offset ,ring-buffer)))
         (declare (type non-negative-fixnum ,position))
         (if (<= (buffered-streams::stream-ring-buffer-stream-start ,ring-buffer)
                 ,position
                 (1- (buffered-streams::stream-ring-buffer-stream-end ,ring-buffer)))
             (prog1 (aref ,buffer (- ,position ,offset))
               (when (>= (- (setf (buffered-streams::stream-ring-buffer-stream-position ,ring-buffer) (1+ ,position)) ,offset) ,buffer-length)
                 (setf (buffered-streams::stream-ring-buffer-buffer-offset ,ring-buffer) (+ ,offset ,buffer-length))))
             (let ((,result (buffered-streams::stream-ring-buffer-read-one ,ring-buffer)))
               (if (eq ,result buffered-streams::+eof+) +input-eof+ ,result)))))))

(deftype character-input-stream ()
  'stream)

(defmethod call-with-input/compile (body (input (eql 'character-input-stream)))
  (with-gensyms (buffer ring-buffer length)
    `(locally (declare (type stream ,input))
       (let* ((,buffer (make-array ,*input-buffer-size* :element-type 'character))
              (,length (length ,buffer))
              (,ring-buffer (buffered-streams::make-stream-ring-buffer :stream ,input :buffer ,buffer)))
         (declare (dynamic-extent ,buffer ,ring-buffer))
         ,(let ((*input-ring-buffer* ring-buffer)
                (*input-buffer* buffer)
                (*input-length* length))
            (funcall body input))))))

(defmethod input-position/compile ((input (eql 'character-input-stream)))
  (input-position/compile 'binary-input-stream))

(defmethod (setf input-position/compile) (value (input (eql 'character-input-stream)))
  (setf (input-position/compile 'binary-input-stream) value))

(defmethod input-read/compile ((input (eql 'character-input-stream)))
  (input-read/compile 'binary-input-stream))
