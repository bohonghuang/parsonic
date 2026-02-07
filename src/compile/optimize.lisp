(in-package #:parsonic)

(defun let->body (form &optional env-bindings)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) (lambda lambda-list &rest body) &rest parsers)
         (assert (eq lambda 'lambda))
         (let* ((args (loop :for binding :in lambda-list
                            :for arg := (list nil '#:unbound nil)
                            :unless (member binding lambda-list-keywords)
                              :collect (replace arg (ensure-list binding))))
                (bindings (nconc args env-bindings)))
           `(,(car form) ,(walk-parsers-in-lambda (rcurry #'let->body bindings) `(lambda ,lambda-list . ,body))
             . ,(mapcar (rcurry #'let->body bindings) parsers))))
        ((parser/unit (name lambda-list) body)
         (let* ((bindings (loop :for (var val) :in env-bindings
                                :collect (list var val nil)))
                (body (let->body body bindings)))
           (loop :for (var val used) :in bindings
                 :for env-binding :in env-bindings
                 :for (env-var env-val) := env-binding
                 :do (assert (eq var env-var)) (assert (eq val env-val))
                 :when used
                   :do (setf (third env-binding) t)
                       (pushnew var lambda-list :key (compose #'car #'ensure-list)))
           `(parser/unit (,name ,lambda-list) ,body)))
        ((parser/let name bindings body)
         (let* ((args (loop :for binding :in bindings
                            :for arg := (list nil '#:unbound nil)
                            :unless (member binding lambda-list-keywords)
                              :collect (replace arg (ensure-list binding))))
                (body (let->body body (append args env-bindings)))
                (args-optimized (loop :for arg :in args
                                      :for (var val) := arg
                                      :unless (eq var val)
                                        :unless (loop :with unused := t
                                                      :for (binding . rest) :on env-bindings
                                                      :for (env-var env-val) := binding
                                                      :when (eq env-var val)
                                                        :if (and (eq env-val var) unused)
                                                          :do (loop :for binding :in rest
                                                                    :for (env-var nil) := binding
                                                                    :when (eq env-var var)
                                                                      :return (setf (third binding) t))
                                                          :and :return binding
                                                        :else
                                                          :do (setf (third binding) t)
                                                          :and :return nil
                                                      :when (and (eq var env-var) (not (eq val env-val)))
                                                        :do (setf unused nil))
                                          :collect arg)))
           (if (and (null name) (equal bindings (mapcar (rcurry #'subseq 0 2) args)))
               (if-let ((bindings (loop :for (var val used) :in args-optimized
                                        :unless (and (not used) (get var 'lexical-store))
                                          :collect (list var val))))
                 `(parser/let ,name ,bindings ,body)
                 body)
               `(parser/let ,name ,bindings ,body))))
        ((t &rest args) (cons (car form) (mapcar (rcurry #'let->body env-bindings) args))))
      form))

(defun satisfies->eql (form)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda #'satisfies->eql function) . ,(mapcar #'satisfies->eql parsers)))
        ((parser/satisfies predicate)
         (if (consp predicate)
             (let ((object (caddr predicate)))
               (if (equal predicate `(curry #'eql ,object))
                   `(parser/eql ,object)
                   form))
             form))
        ((t &rest args) (cons (car form) (mapcar #'satisfies->eql args))))
      form))

(defun ors->or (form)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda #'ors->or function) . ,(mapcar #'ors->or parsers)))
        ((parser/or &rest parsers)
         `(parser/or
           . ,(loop :for parser :in parsers
                    :for form := (ors->or parser)
                    :if (eq (car form) 'parser/or)
                      :append (cdr form)
                    :else
                      :collect form)))
        ((t &rest args) (cons (car form) (mapcar #'ors->or args))))
      form))

(defun conses->list (form)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda #'conses->list function) . ,(mapcar #'conses->list parsers)))
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

(defun apply->funcall (form)
  (if (consp form)
      (destructuring-case form
        ((parser/apply function parser)
         (if (eq (car parser) 'parser/list)
             `(parser/funcall ,(walk-parsers-in-lambda #'apply->funcall function) . ,(mapcar #'apply->funcall (cdr parser)))
             `(parser/apply ,(walk-parsers-in-lambda #'apply->funcall function) ,(apply->funcall parser))))
        ((t &rest args) (cons (car form) (mapcar #'apply->funcall args))))
      form))

(defun body-declarations (body)
  (values
   (loop :for (declaration . rest) :on body
         :while (and (listp declaration) (eq (car declaration) 'declare))
         :do (setf body rest)
         :append (cdr declaration))
   body))

(defun flatmap->map (form)
  (if (consp form)
      (destructuring-case form
        ((parser/funcall function &rest parsers)
         (destructuring-ecase function
           ((lambda lambda-list &rest body)
            (multiple-value-bind (declarations body) (body-declarations body)
              (if (and (equal body `((with-codegen (parser/constantly ,(ignore-errors (second (second (first body))))))))
                       (null (intersection lambda-list lambda-list-keywords)))
                  (let* ((value (second (second (first body))))
                         (args lambda-list)
                         (ignored-args (loop :for (type . args) :in declarations :when (eq type 'ignore) :append args))
                         (declarations (remove 'ignore declarations :key #'car)))
                    `(parser/filter
                      (lambda ,(loop :for arg :in args :if (member arg ignored-args) :collect nil :else :collect arg)
                        ,@(when declarations `((declare . ,declarations)))
                        ,value)
                      . ,(mapcar #'flatmap->map parsers)))
                  `(parser/funcall ,(walk-parsers-in-lambda #'flatmap->map function) . ,(mapcar #'flatmap->map parsers)))))))
        ((parser/apply function parser) `(parser/apply ,(walk-parsers-in-lambda #'flatmap->map function) ,(flatmap->map parser)))
        ((t &rest args) (declare (ignore args)) (cons (car form) (mapcar #'flatmap->map (cdr form)))))
      form))

(defun flatmap->let (form)
  (if (consp form)
      (destructuring-case form
        ((parser/funcall function &rest parsers)
         (destructuring-ecase function
           ((lambda lambda-list &rest body)
            (multiple-value-bind (declarations body) (body-declarations body)
              (if (and (equal body `((with-codegen ,(ignore-errors (second (first body))))))
                       (every (compose (curry #'eq 'parser/constantly) #'car) parsers)
                       (notany #'cdr declarations)
                       (null (intersection lambda-list lambda-list-keywords)))
                  `(parser/let nil ,(mapcar #'list lambda-list (mapcar #'second parsers)) ,(flatmap->let (second (first body))))
                  `(parser/funcall ,(walk-parsers-in-lambda #'flatmap->let function) . ,(mapcar #'flatmap->let parsers)))))))
        ((parser/apply function parser) `(parser/apply ,(walk-parsers-in-lambda #'flatmap->let function) ,(flatmap->let parser)))
        ((t &rest args) (declare (ignore args)) (cons (car form) (mapcar #'flatmap->let (cdr form)))))
      form))

(defparameter *inline-sequence-eql-threshold* 8)

(defun eql-list->eql* (form)
  (if (consp form)
      (destructuring-case form
        (((parser/funcall parser/apply) function &rest parsers)
         `(,(car form) ,(walk-parsers-in-lambda #'eql-list->eql* function) . ,(mapcar #'eql-list->eql* parsers)))
        ((parser/list &rest parsers)
         (if (and (every (curry #'eq 'parser/eql) (mapcar #'car parsers))
                  (> (length parsers) *inline-sequence-eql-threshold*))
             `(parser/eql* ,(mapcar #'second parsers))
             (cons (car form) (mapcar #'eql-list->eql* (cdr form)))))
        ((t &rest args) (declare (ignore args))
         (cons (car form) (mapcar #'eql-list->eql* (cdr form)))))
      form))

(defparameter *optimize-passes* '(let->body satisfies->eql ors->or
                                  conses->list apply->funcall
                                  flatmap->map flatmap->let let->body
                                  or->cse or->trie eql-list->eql*))

(defun optimize/compile (initial)
  (loop :for form := initial :then (funcall pass form)
        :for pass :in *optimize-passes*
        :finally (return form)))
