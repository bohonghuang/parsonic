(in-package #:parsonic)

(defvar *compute-bounds* #'compute-bounds/eval)
(declaim (type function *compute-bounds*))

(defun compute-bounds (form)
  (funcall *compute-bounds* form))
