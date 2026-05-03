(in-package #:parsonic)

(defvar *input-buffer*)
(defvar *input-ring-buffer*)

(defparameter *input-buffer-size* 4096)

(defun ring-buffer-read/compile ()
  (let ((buffer *input-buffer*)
        (buffer-length *input-length*)
        (ring-buffer *input-ring-buffer*))
    (with-gensyms (position offset)
      `(let ((,position (buffered-streams::stream-ring-buffer-stream-position ,ring-buffer))
             (,offset (buffered-streams::stream-ring-buffer-buffer-offset ,ring-buffer)))
         (declare (type non-negative-fixnum ,position))
         (if (<= (buffered-streams::stream-ring-buffer-stream-start ,ring-buffer)
                 ,position
                 (1- (buffered-streams::stream-ring-buffer-stream-end ,ring-buffer)))
             (prog1 (aref ,buffer (mod (- ,position ,offset) ,buffer-length))
               (setf (buffered-streams::stream-ring-buffer-stream-position ,ring-buffer) (1+ ,position)))
             (buffered-streams::stream-ring-buffer-read-element ,ring-buffer +input-eof+))))))

(defun ring-buffer-position/compile ()
  `(buffered-streams::stream-ring-buffer-stream-position ,*input-ring-buffer*))

(defun (setf ring-buffer-position/compile) (value)
  (let ((ring-buffer *input-ring-buffer*))
    (once-only (value)
      `(if (<= (buffered-streams::stream-ring-buffer-stream-start ,ring-buffer)
               ,value
               (1- (buffered-streams::stream-ring-buffer-stream-end ,ring-buffer)))
           (setf (buffered-streams::stream-ring-buffer-stream-position ,ring-buffer) ,value)
           (buffered-streams::stream-ring-buffer-seek ,ring-buffer ,value)))))

(deftype unbuffered-binary-input-stream ()
  '(and binary-input-stream (not buffered-streams:buffered-binary-input-stream)))

(defmethod call-with-input/compile (body (input (eql 'unbuffered-binary-input-stream)))
  (with-gensyms (buffer ring-buffer length stream)
    `(locally (declare (type stream ,input))
       (let* ((,buffer (make-array ,*input-buffer-size* :element-type '(unsigned-byte 8)))
              (,length (length ,buffer))
              (,stream (make-instance 'buffered-streams:buffered-binary-input-stream :stream ,input :buffer ,buffer))
              (,ring-buffer (buffered-streams::stream-buffer ,stream)))
         (declare (dynamic-extent ,buffer ,ring-buffer))
         ,(let ((*input-ring-buffer* ring-buffer)
                (*input-buffer* buffer)
                (*input-length* length))
            (funcall body input))))))

(defmethod input-position/compile ((input (eql 'unbuffered-binary-input-stream)))
  (ring-buffer-position/compile))

(defmethod (setf input-position/compile) (value (input (eql 'unbuffered-binary-input-stream)))
  (setf (ring-buffer-position/compile) value))

(defmethod input-read/compile ((input (eql 'unbuffered-binary-input-stream)))
  `(the (or (unsigned-byte 8) (eql ,+input-eof+)) ,(ring-buffer-read/compile)))

(deftype unbuffered-character-input-stream ()
  '(and character-input-stream (not buffered-streams:buffered-character-input-stream)))

(defmethod call-with-input/compile (body (input (eql 'unbuffered-character-input-stream)))
  (with-gensyms (buffer ring-buffer length stream)
    `(locally (declare (type stream ,input))
       (let* ((,buffer (make-array ,*input-buffer-size* :element-type 'character))
              (,length (length ,buffer))
              (,stream (make-instance 'buffered-streams:buffered-character-input-stream :stream ,input :buffer ,buffer))
              (,ring-buffer (buffered-streams::stream-buffer ,stream)))
         (declare (dynamic-extent ,buffer))
         ,(let ((*input-ring-buffer* ring-buffer)
                (*input-buffer* buffer)
                (*input-length* length))
            (funcall body input))))))

(defmethod input-position/compile ((input (eql 'unbuffered-character-input-stream)))
  (ring-buffer-position/compile))

(defmethod (setf input-position/compile) (value (input (eql 'unbuffered-character-input-stream)))
  (setf (ring-buffer-position/compile) value))

(defmethod input-read/compile ((input (eql 'unbuffered-character-input-stream)))
  `(the (or character (eql ,+input-eof+)) ,(ring-buffer-read/compile)))
