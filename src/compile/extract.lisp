(in-package #:parseq)

(defvar *parser-units*)
(defvar *parser-unit-parent* nil)

(defstruct parser-unit
  (parents nil :type list)
  (children nil :type list)
  (tree nil :type list)
  (count 0 :type non-negative-fixnum))

(defun parser-tree-cost (tree)
  (typecase tree
    ((cons symbol cons)
     (+ (if (eql (search (string '#:parser/) (symbol-name (car tree))) 0) 1 0)
        (parser-tree-cost (car tree)) (parser-tree-cost (cdr tree))))
    (cons (+ (parser-tree-cost (car tree)) (parser-tree-cost (cdr tree))))
    (t 0)))

(defun parser-unit-cost (unit)
  (parser-tree-cost (parser-unit-tree unit)))

(defun parser-unit-mapc (function unit)
  (funcall function unit)
  (mapc (lambda (unit) (parser-unit-mapc function unit)) (parser-unit-children unit)))

(defun parser-unit-walk (form)
  (if (consp form)
      (destructuring-case form
        ((parser/unit signature parser)
         (let* ((unit (ensure-gethash signature *parser-units* (make-parser-unit)))
                (tree (let ((*parser-unit-parent* (when (zerop (parser-unit-count unit)) unit)))
                        (parser-unit-walk parser))))
           (when *parser-unit-parent*
             (push unit (parser-unit-children *parser-unit-parent*))
             (pushnew *parser-unit-parent* (parser-unit-parents unit)))
           (incf (parser-unit-count unit))
           (setf (parser-unit-tree unit) (or (parser-unit-tree unit) `(parser/unit ,signature ,tree)))))
        ((t &rest args)
         (declare (ignore args))
         (cons (car form) (mapcar #'parser-unit-walk (cdr form)))))
      form))

(defparameter *parser-unit-inline-threshold-count* 2)
(defparameter *parser-unit-inline-threshold-cost* 16)
(defparameter *parser-unit-inline-threshold-total-cost* 64)

(defun extract/compile (form)
  (loop :with units := (let ((*parser-units* (make-hash-table :test #'equal)))
                         (setf form (parser-unit-walk form))
                         (hash-table-values *parser-units*))
        :and functions := nil
        :for (unit) := (setf units (loop :for unit :in (sort units #'> :key #'parser-unit-cost)
                                         :for count := (parser-unit-count unit)
                                         :and cost := (parser-unit-cost unit)
                                         :when (and (>= count *parser-unit-inline-threshold-count*)
                                                    (>= cost *parser-unit-inline-threshold-cost*)
                                                    (>= (* count cost) *parser-unit-inline-threshold-cost*))
                                           :collect unit))
        :while unit
        :do (destructuring-bind (parser/unit (name lambda-list) parser) (parser-unit-tree unit)
              (assert (eq parser/unit 'parser/unit))
              (let ((name (gensym (princ-to-string name)))
                    (args (lambda-list-arguments lambda-list)))
                (push (list name args parser) functions)
                (let ((count (parser-unit-count unit)))
                  (parser-unit-mapc (lambda (unit) (decf (parser-unit-count unit) count)) unit))
                (loop :for parent :in (parser-unit-parents unit)
                      :do (deletef (parser-unit-children parent) unit))
                (setf (car (parser-unit-tree unit)) 'parser/call
                      (cdr (parser-unit-tree unit)) (cons name args))))
        :finally (return (values form functions))))
