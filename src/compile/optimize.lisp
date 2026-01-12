(in-package #:parseq)

(defun conses->list (form)
  (if (consp form)
      (destructuring-case form
        ((parser/funcall function &rest parsers)
         `(parser/funcall ,function . ,(mapcar #'conses->list parsers)))
        ((parser/cons car cdr)
         (let ((rest (conses->list cdr)))
           (destructuring-case rest
             ((parser/list &rest rest)
              `(parser/list ,(conses->list car) . ,rest))
             ((t &rest args) (declare (ignore args))
              `(parser/cons ,(conses->list car) ,rest)))))
        ((parser/constantly value) (if (eq value nil) '(parser/list) form))
        ((t &rest args) (cons (car form) (mapcar #'conses->list args))))
      form))

(defun flatmap->map (form)
  (if (consp form)
      (destructuring-case form
        ((parser/funcall function &rest parsers)
         (destructuring-ecase function
           ((lambda lambda-list &rest body)
            (destructuring-case (ensure-list (lastcar body))
              ((parser/constantly value &aux (parser (first parsers)))
               (destructuring-ecase value
                 ((destructuring-bind args list &rest body &aux (parsers (cdr parser)))
                  (assert (equal (list list) lambda-list))
                  (assert (eq (car parser) 'parser/list))
                  (let ((ignored-args (loop :for form := (car body)
                                            :while (consp form)
                                            :while (eq (car form) 'declare)
                                            :append (assoc-value (cdr form) 'ignore)
                                            :do (setf body (cdr body)))))
                    `(parser/filter
                      (lambda ,(loop :for arg :in args :if (member arg ignored-args) :collect nil :else :collect arg)
                        ,@body)
                      . ,(mapcar #'flatmap->map parsers))))))
              ((t &rest args) (declare (ignore args)) `(parser/funcall ,function . ,(mapcar #'flatmap->map parsers)))))))
        ((t &rest args) (declare (ignore args)) (cons (car form) (mapcar #'flatmap->map (cdr form)))))
      form))

(defparameter *inline-sequence-eql-threshold* 8)

(defun eql-list->eql* (form)
  (if (consp form)
      (destructuring-case form
        ((parser/list &rest parsers)
         (if (and (every (curry #'eq 'parser/eql) (mapcar #'car parsers))
                  (> (length parsers) *inline-sequence-eql-threshold*))
             `(parser/eql* ,(mapcar #'second parsers))
             (cons (car form) (mapcar #'eql-list->eql* (cdr form)))))
        ((t &rest args) (declare (ignore args))
         (cons (car form) (mapcar #'eql-list->eql* (cdr form)))))
      form))

(defparameter *optimize-passes* '(or->trie conses->list flatmap->map eql-list->eql*))

(defun optimize/compile (initial)
  (loop :for form := initial :then (funcall pass form)
        :for pass :in *optimize-passes*
        :finally (return form)))
