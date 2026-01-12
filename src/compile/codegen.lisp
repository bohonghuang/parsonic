(in-package #:parseq)

(defvar *codegen-input*)
(defvar *codegen-blocks*)
(defvar *codegen-list-vars*)
(defvar *codegen-cons*)
(defvar *codegen-make-list*)
(defvar *codegen-labels*)

(defparameter *merge-stack-list-allocation-p* nil)
(defparameter *pseudo-parser-object-p* t)

(defun function-identity-p (form)
  (destructuring-case form
    ((lambda lambda-list &rest body) (equal lambda-list body))))

(defun codegen-parse-error (&optional (value (input-position/compile *codegen-input*)))
  `(values ,value t))

(defun call-with-fresh-stack (body)
  (with-gensyms (list block block-outer result errorp)
    (let ((*codegen-list-vars* nil))
      (let ((null '#:null)
            (body (let ((*codegen-blocks* (cons block *codegen-blocks*))) (funcall body))))
        `(let* ,@(if *merge-stack-list-allocation-p*
                     `(((,list (make-list ,(reduce #'+ *codegen-list-vars* :key (compose (curry #'max 0) #'cdr))))
                        . ,(loop :for (var . size) :in *codegen-list-vars*
                                 :if (plusp size)
                                   :collect `(,var (shiftf ,list (cdr (nthcdr ,(1- size) ,list)) nil))
                                 :else
                                   :collect `(,var nil)))
                       (declare (dynamic-extent ,list)))
                     `(,(loop :for (var . size) :in *codegen-list-vars*
                              :if (plusp size)
                                :collect `(,var (make-list ,size))
                              :else :if (minusp size)
                                :collect `(,var ,(loop :for form := nil :then (funcall *codegen-cons* nil form)
                                                       :repeat (abs size) :finally (return form)))
                              :else
                                :collect `(,var nil))
                       (declare (dynamic-extent . ,(mapcar #'car (remove-if-not #'plusp *codegen-list-vars* :key #'cdr))))))
           (let* ((,errorp ',null)
                  (,result (block ,block-outer (setf ,errorp (block ,block (return-from ,block-outer ,body))))))
             ,@(loop :for (var . size) :in *codegen-list-vars*
                     :unless (plusp size) :collect (funcall *codegen-cons* (if (zerop size) var `(subseq ,var 0 ,(abs size)))))
             (unless (eq ,errorp ',null)
               (return-from ,(car *codegen-blocks*) ,(codegen-parse-error result)))
             ,result))))))

(defmacro with-fresh-stack (form)
  `(call-with-fresh-stack (lambda () ,form)))

(defun codegen-expand (form)
  (optimize/compile (expand/compile form)))

(defun codegen (form)
  (flet ((ignore-results-p () (ignore-errors (funcall *codegen-make-list*)))
         (ignore-results () (let ((make-list *codegen-make-list*))
                              (lambda (&optional size)
                                (unless size make-list)))))
    (destructuring-ecase form
      ((parser/satisfies predicate)
       (with-gensyms (result)
         `(let ((,result ,(input-read/compile *codegen-input*)))
            (if (funcall ,predicate ,result)
                ,(unless (ignore-results-p) result)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error))))))
      ((parser/eql object)
       (with-gensyms (result)
         `(let ((,result ,(input-read/compile *codegen-input*)))
            (if (eql ,result ,object)
                ,(unless (ignore-results-p) object)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error))))))
      ((parser/eql* object)
       (with-gensyms (expected result)
         `(loop :for ,expected :across ,(if (every #'characterp object) (coerce object 'simple-string) (coerce object 'simple-vector))
                :for ,result := ,(input-read/compile *codegen-input*)
                :unless (eql ,result ,expected)
                  :do (return-from ,(car *codegen-blocks*) ,(codegen-parse-error))
                :finally (return ',(unless (ignore-results-p) object)))))
      ((parser/list &rest parsers)
       (if parsers
           (with-gensyms (var list)
             (if-let ((make-list (funcall *codegen-make-list* (length parsers))))
               `(let* ((,list ,make-list)
                       (,var ,list))
                  ,@(loop :for parser :in parsers
                          :nconc `((setf (car ,var) ,(codegen parser))
                                   (pop ,var)))
                  ,list)
               `(progn ,@(mapcar #'codegen parsers) nil)))
           (codegen '(parser/constantly nil))))
      ((parser/cons car cdr)
       (with-gensyms (cons)
         (if-let ((make-list (funcall *codegen-make-list* 1)))
           `(let ((,cons ,make-list))
              (setf (car ,cons) ,(codegen car)
                    (cdr ,cons) ,(codegen cdr))
              ,cons)
           `(progn ,(codegen car) ,(codegen cdr) nil))))
      ((parser/or &rest parsers)
       (if parsers
           (with-gensyms (position)
             `(let ((,position ,(input-position/compile *codegen-input*)))
                ,(labels ((recur (parsers)
                            (with-gensyms (block)
                              (let ((*codegen-blocks* (cons block *codegen-blocks*)))
                                `(block ,block
                                   ,@(when (cdr parsers)
                                       `(,(recur (cdr parsers))
                                         ,(setf (input-position/compile *codegen-input*) position)))
                                   (return-from ,(lastcar *codegen-blocks*) ,(codegen (car parsers))))))))
                   (with-gensyms (short-circut)
                     `(block ,short-circut
                        ,(let ((*codegen-blocks* (list short-circut)))
                           (recur (reverse parsers)))
                        (return-from ,(car *codegen-blocks*) ,(codegen-parse-error)))))))
           `(return-from ,(car *codegen-blocks*) ,(codegen-parse-error))))
      ((parser/filter (lambda lambda-list &rest body) &rest args)
       (assert (eq lambda 'lambda))
       (if (ignore-results-p)
           (codegen `(parser/list . ,args))
           (flet ((body (function)
                    (loop :for name :in lambda-list
                          :for arg :in args
                          :if name
                            :collect (gensym) :into vars
                            :and :collect `(setf ,(lastcar vars) ,(codegen arg)) :into body
                          :else
                            :collect (let ((*codegen-make-list* (ignore-results)))
                                       (codegen arg))
                              :into body
                          :finally (return `(let ,vars ,@body (,@function . ,vars))))))
             (let ((function `(lambda ,(remove nil lambda-list) . ,body)))
               (if (function-identity-p function)
                   (body '(progn))
                   (with-fresh-stack (body `(,function))))))))
      ((parser/unit signature parser)
       (declare (ignore signature))
       (codegen parser))
      ((parser/funcall function &rest args)
       (destructuring-ecase function
         ((lambda lambda-list &rest body)
          (declare (ignore body))
          (assert (= (length lambda-list) (length args)))
          (let* ((function (labels ((walk (form)
                                      (typecase form
                                        (null form)
                                        (proper-list
                                         (destructuring-case form
                                           ((with-codegen parser)
                                            (if *pseudo-parser-object-p*
                                                (codegen parser)
                                                `(lambda () ,(codegen parser))))
                                           ((t &rest args) (cons (car form) (mapcar #'walk args)))))
                                        (t form))))
                             (walk function)))
                 (result `(,function . ,(let ((*codegen-make-list* (or (ignore-results-p) *codegen-make-list*)))
                                          (mapcar #'codegen args)))))
            (if *pseudo-parser-object-p* result `(funcall ,result))))))
      ((parser/let name lambda-list body)
       (multiple-value-bind (lambda-list args)
           (if (intersection lambda-list lambda-list-keywords)
               (loop :with required
                     :for (arg . rest) :on lambda-list
                     :when (eq arg '&initial)
                       :return (values
                                args
                                (progn
                                  (assert (>= (length rest) (length required)))
                                  (assert (every #'eq (mapcar #'first rest) required))
                                  (mapcar #'second rest)))
                     :when (member arg lambda-list-keywords)
                       :do (setf required (copy-list args))
                     :collect arg :into args)
               (values (mapcar #'first lambda-list) (mapcar #'second lambda-list)))
         (if name
             (funcall *codegen-labels*
                      (list (list name lambda-list
                                  (let ((*codegen-blocks* (list name)))
                                    (with-fresh-stack (codegen body)))))
                      (codegen `(parser/call ,name . ,args)))
             `((lambda ,lambda-list ,(codegen body)) . ,args))))
      ((parser/call name &rest args)
       (with-gensyms (result errorp)
         `(multiple-value-bind (,result ,errorp) (,name . ,args)
            (if ,errorp
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error))
                ,result))))
      ((parser/constantly object) object)
      ((parser/rep parser &optional (from 0) (to most-positive-fixnum))
       (with-gensyms (block block-outer counter list position cons-root cons)
         (push (cons list 0) *codegen-list-vars*)
         (if-let ((make-list (funcall *codegen-make-list* 1)))
           `(let ((,cons-root ,make-list)
                  (,counter 0)
                  (,position ,(input-position/compile *codegen-input*)))
              (declare (type non-negative-fixnum ,counter))
              (block ,block-outer
                (loop :named ,block
                      :initially ,(funcall *codegen-cons* list) (setf (cdr ,cons-root) nil)
                      :for ,cons := ,cons-root :then (cdr ,cons)
                      :do (setf (cdr ,cons) ,(funcall
                                              *codegen-cons*
                                              (let ((*codegen-blocks* (cons block *codegen-blocks*)))
                                                (codegen parser))
                                              nil)
                                ,position ,(input-position/compile *codegen-input*)
                                ,counter (1+ ,counter))
                      :repeat ,to
                      :finally (return-from ,block-outer))
                ,(setf (input-position/compile *codegen-input*) position))
              (setf ,list (cdr ,cons-root))
              (unless (>= ,counter ,from)
                ,(funcall *codegen-cons* list)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error)))
              ,list)
           `(let ((,counter 0)
                  (,position ,(input-position/compile *codegen-input*)))
              (declare (type non-negative-fixnum ,counter))
              (block ,block-outer
                (loop :named ,block
                      :do ,(let ((*codegen-blocks* (cons block *codegen-blocks*)))
                             (codegen parser))
                          (setf ,position ,(input-position/compile *codegen-input*)
                                ,counter (1+ ,counter))
                      :repeat ,to
                      :finally (return-from ,block-outer))
                ,(setf (input-position/compile *codegen-input*) position))
              (unless (>= ,counter ,from)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error)))))))
      ((parser/case &rest branches)
       (with-gensyms (position)
         (let* ((default (car (assoc-value branches '_)))
                (body `(case ,(input-read/compile *codegen-input*)
                         ,@(loop :for (key parser) :in branches
                                 :unless (eq key '_) :collect `((,key) ,(codegen parser)))
                         (t . ,(if default
                                   `(,(setf (input-position/compile *codegen-input*) position) ,(codegen default))
                                   `((return-from ,(car *codegen-blocks*) ,(codegen-parse-error))))))))
           (if default
               `(let ((,position ,(input-position/compile *codegen-input*)))
                  (declare (ignorable ,position))
                  ,body)
               body)))))))

(defmethod expand-expr/compile ((op (eql 'funcall)) &rest args)
  (destructuring-bind (function &rest parsers) args
    `(parser/funcall
      ,(labels ((walk (form)
                  (typecase form
                    (null form)
                    (proper-list
                     (destructuring-case form
                       ((parser parser) `(with-codegen ,(codegen-expand parser)))
                       ((t &rest args) (cons (car form) (mapcar #'walk args)))))
                    (t form))))
         (walk function))
      . ,(mapcar #'expand parsers))))
