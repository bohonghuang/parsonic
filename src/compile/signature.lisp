(in-package #:parsonic)

(defun parser-signature-trim (form)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda #'parser-signature-trim function) . ,(mapcar #'parser-signature-trim parsers)))
        ((parser/let bindings body)
         (if-let ((bindings (remove-if-not (compose #'constantp #'second) bindings)))
           `(parser/let ,bindings ,(parser-signature-trim body))
           (parser-signature-trim body)))
        ((parser/unit &rest args)
         (declare (ignore args))
         (butlast form))
        ((t &rest args) (cons (car form) (mapcar #'parser-signature-trim args))))
      form))

(defun parser-signature (form)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((recur (form)
               (typecase form
                 (symbol (if (symbol-package form) form (ensure-gethash form table (1+ (hash-table-count table)))))
                 (cons (cons (recur (car form)) (recur (cdr form))))
                 (t form))))
      (let ((form (parser-signature-trim (expand form))))
        (case (car form)
          ((curry rcurry))
          (t (recur form)))))))

(defun parser-signature-name (object)
  (typecase object
    ((cons symbol list)
     (destructuring-case object
       ((parser/unit (name lambda-list))
        (declare (ignore lambda-list))
        (parser-signature-name name))
       ((parser/let bindings body)
        (format nil "~A~{<~A>~}" (parser-signature-name body) (mapcar #'second bindings)))
       ((t &rest args) (declare (ignore args))
        (multiple-value-call #'format nil "~A~{[~A]~}"
          (if-let ((name (parser-symbol-name (car object))))
            (values name (mapcar (compose #'parser-signature-name #'car #'ensure-list) (cdr object)))
            (values (car object) (mapcar #'parser-signature-name (cdr object))))))))
    (cons (format nil "~{~A~^_~}" (mapcar #'parser-signature-name object)))
    (symbol (symbol-name (or (parser-symbol-name object) object)))
    (t (princ-to-string object))))

(defun parser-signature-gensym (signature)
  (gensym (format nil "~A_" (parser-signature-name signature))))
