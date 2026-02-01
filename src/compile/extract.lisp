(in-package #:parsonic)

(defun lexical->unit-args (form &optional blocks)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda (rcurry #'lexical->unit-args blocks) function)
           . ,(mapcar (rcurry #'lexical->unit-args blocks) parsers)))
        ((parser/unit signature body)
         (declare (ignore signature))
         (lexical->unit-args body (cons form blocks))
         form)
        ((parser/let name bindings body)
         (declare (ignore name))
         (loop :for binding :in bindings
               :for (nil val) := (ensure-list binding)
               :when (and val (symbolp val))
                 :do (loop :for block :in blocks
                           :for (type signature bindings) := block
                           :if (eq type 'parser/unit)
                             :collect signature :into signatures
                           :else
                             :if (eq type 'parser/let)
                               :if (find val bindings :key (compose #'car #'ensure-list))
                                 :return (loop :for signature :in signatures
                                               :do (pushnew val (second signature) :key (compose #'car #'ensure-list)))))
         (lexical->unit-args body (cons form blocks))
         form)
        ((t &rest args) (cons (car form) (mapcar (rcurry #'lexical->unit-args blocks) args))))
      form))

(defvar *intermediate-unit-p*)

(defun remove-intermediate-units (form)
  (if (boundp '*intermediate-unit-p*)
      (if (consp form)
          (destructuring-case form
            (((parser/cons parser/list parser/rep) &rest args)
             (when args (setf *intermediate-unit-p* t))
             (cons (car form) (mapcar #'remove-intermediate-units args)))
            (((parser/funcall parser/apply) function &rest parsers)
             (let ((*intermediate-unit-p* nil))
               `(,(car form) ,(walk-parsers-in-lambda
                               (lambda (form)
                                 (let ((*intermediate-unit-p* nil))
                                   (remove-intermediate-units form)))
                               function)
                 . ,(mapcar #'remove-intermediate-units parsers))))
            ((parser/filter (lambda lambda-list &rest body) &rest parsers)
             (assert (eq lambda 'lambda))
             `(,(car form) (lambda ,lambda-list . ,body)
               . ,(loop :with function := `(lambda ,(remove nil lambda-list) . ,body)
                        :with variable-arg-index := (when (function-identity-p function) (position nil lambda-list :test-not #'eq))
                        :for parser :in parsers
                        :for index :from 0
                        :if (eql index variable-arg-index)
                          :collect (remove-intermediate-units parser)
                        :else
                          :collect (let ((*intermediate-unit-p* nil))
                                     (remove-intermediate-units parser)))))
            ((parser/unit signature body)
             (let ((*intermediate-unit-p* nil))
               (let ((body (remove-intermediate-units body)))
                 (if *intermediate-unit-p* body `(parser/unit ,signature ,body)))))
            ((t &rest args) (cons (car form) (mapcar #'remove-intermediate-units args))))
          form)
      (let ((*intermediate-unit-p* nil))
        (remove-intermediate-units form))))

(defvar *parser-units*)
(defvar *parser-unit-parent* nil)

(defstruct parser-unit
  (parents nil :type list)
  (children nil :type list)
  (count 0 :type non-negative-fixnum)
  (tree nil :type list)
  (tree-cost 0 :type non-negative-fixnum))

(defun parser-unit-calculate-tree-cost (unit &optional (tree (parser-unit-tree unit)))
  (typecase tree
    ((cons symbol list)
     (destructuring-case tree
       ((parser/unit signature body)
        (1+ (if (eq (parser-unit-tree unit) tree)
                (parser-unit-calculate-tree-cost unit body)
                (parser-unit-cost (find signature (parser-unit-children unit) :key (compose #'second #'parser-unit-tree) :test #'equal)))))
       ((t &rest args)
        (declare (ignore args))
        (+ (if (eql (search (string '#:parser/) (symbol-name (car tree))) 0) 1 0)
           (parser-unit-calculate-tree-cost unit (car tree)) (parser-unit-calculate-tree-cost unit (cdr tree))))))
    (cons (+ (parser-unit-calculate-tree-cost unit (car tree)) (parser-unit-calculate-tree-cost unit (cdr tree))))
    (t 0)))

(defun parser-unit-cost (unit)
  (if (plusp (parser-unit-tree-cost unit))
      (parser-unit-tree-cost unit)
      (setf (parser-unit-tree-cost unit) (parser-unit-calculate-tree-cost unit))))

(defun (setf parser-unit-cost) (value unit)
  (assert (plusp value))
  (setf (parser-unit-tree-cost unit) value))

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

(defparameter *parser-unit-extract-threshold-count* 2)
(defparameter *parser-unit-extract-threshold-cost* 16)
(defparameter *parser-unit-extract-threshold-total-cost* 64)
(defparameter *parser-unit-extract-recursive-p* t)

(defun extract/compile (form)
  (loop :with units := (let ((*parser-units* (make-hash-table :test #'equal)))
                         (setf form (parser-unit-walk (lexical->unit-args (remove-intermediate-units form))))
                         (hash-table-values *parser-units*))
        :and functions := nil
        :for (unit) := (setf units (loop :for unit :in (sort units #'> :key #'parser-unit-cost)
                                         :for count := (parser-unit-count unit)
                                         :and cost := (parser-unit-cost unit)
                                         :when (and (>= count *parser-unit-extract-threshold-count*)
                                                    (>= cost *parser-unit-extract-threshold-cost*)
                                                    (>= (* count cost) *parser-unit-extract-threshold-cost*))
                                           :collect unit))
        :while unit
        :do (destructuring-bind (parser/unit (name lambda-list) parser) (parser-unit-tree unit)
              (assert (eq parser/unit 'parser/unit))
              (let ((name (gensym (format nil "~{~A~}" (mapcar (compose #'car #'ensure-list) name))))
                    (args (lambda-list-arguments lambda-list)))
                (push (list name args parser) functions)
                (let ((count (if *parser-unit-extract-recursive-p* (1- (parser-unit-count unit)) (parser-unit-count unit))))
                  (parser-unit-mapc (lambda (unit) (decf (parser-unit-count unit) count)) unit))
                (assert (< (parser-unit-count unit) *parser-unit-extract-threshold-count*))
                (labels ((recur (unit size-reduced)
                           (when (plusp size-reduced)
                             (loop :for parent :in (parser-unit-parents unit)
                                   :for total-size-reduced := (loop :for child :in (parser-unit-children parent)
                                                                    :when (eq child unit) :sum size-reduced)
                                   :do (decf (parser-unit-cost parent) total-size-reduced)
                                       (recur parent total-size-reduced)))))
                  (recur unit (1- (parser-unit-cost unit))))
                (loop :for parent :in (parser-unit-parents unit)
                      :do (deletef (parser-unit-children parent) unit))
                (setf (car (parser-unit-tree unit)) 'parser/call
                      (cdr (parser-unit-tree unit)) (cons name args))))
        :finally (return (values form functions))))
