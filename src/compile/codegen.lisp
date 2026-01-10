(in-package #:parseq)

(defvar *codegen-input*)
(defvar *codegen-blocks*)
(defvar *codegen-list-vars*)
(defvar *codegen-cons*)
(defvar *codegen-make-list*)
(defvar *codegen-labels*)

(defparameter *dynamic-stack-list-allocation-p* t)
(defparameter *merge-stack-list-allocation-p* nil)
(defparameter *pseudo-parser-object-p* t)

(defun function-identity-p (form)
  (destructuring-case form
    ((lambda lambda-list &rest body) (equal lambda-list body))))

(defun call-with-fresh-stack (body)
  (let ((codegen-list-vars (boundp '*codegen-list-vars*)))
    (prog1 (let ((*codegen-list-vars* nil))
             (let ((body (funcall body)))
               (when codegen-list-vars
                 (unless *dynamic-stack-list-allocation-p*
                   (setf *codegen-list-vars* (loop :initially (setf codegen-list-vars nil)
                                                   :for cons :in *codegen-list-vars*
                                                   :if (zerop (cdr cons))
                                                     :collect cons
                                                   :else
                                                     :do (push cons codegen-list-vars)))))
               (with-gensyms (list)
                 `(let* ,@(if *merge-stack-list-allocation-p*
                              `(((,list (make-list ,(reduce #'+ *codegen-list-vars* :key #'cdr)))
                                 . ,(loop :for (var . size) :in *codegen-list-vars*
                                          :if (zerop size)
                                            :collect `(,var nil)
                                          :else
                                            :collect `(,var (shiftf ,list (cdr (nthcdr ,(1- size) ,list)) nil))))
                                (declare (dynamic-extent ,list)))
                              `(,(loop :for (var . size) :in *codegen-list-vars*
                                       :collect `(,var (make-list ,size)))
                                (declare (dynamic-extent . ,(mapcar #'car *codegen-list-vars*)))))
                    (prog1 ,body
                      ,@(loop :for (var . size) :in *codegen-list-vars*
                              :when (zerop size)
                                :collect (funcall *codegen-cons* var)))))))
      (when (consp codegen-list-vars)
        (setf *codegen-list-vars* (nconc codegen-list-vars *codegen-list-vars*))))))

(defmacro with-fresh-stack (form)
  `(call-with-fresh-stack (lambda () ,form)))

(defun codegen-expand (form)
  (optimize/compile (expand/compile form)))

(defun codegen (form)
  (flet ((parser-error () `(values ,(input-position/compile *codegen-input*) t))
         (ignore-results-p () (ignore-errors (funcall *codegen-make-list*)))
         (ignore-results () (let ((make-list *codegen-make-list*))
                              (lambda (&optional size)
                                (unless size make-list)))))
    (destructuring-ecase form
      ((parser/satisfies predicate)
       (with-gensyms (result)
         `(let ((,result ,(input-read/compile *codegen-input*)))
            (if (funcall ,predicate ,result)
                ,result
                (return-from ,(car *codegen-blocks*) ,(parser-error))))))
      ((parser/eql object)
       (with-gensyms (result)
         `(let ((,result ,(input-read/compile *codegen-input*)))
            (if (eql ,result ,object)
                ,object
                (return-from ,(car *codegen-blocks*) ,(parser-error))))))
      ((parser/eql* object)
       (with-gensyms (expected result)
         `(loop :for ,expected :across ,(if (every #'characterp object) (coerce object 'simple-string) (coerce object 'simple-vector))
                :for ,result := ,(input-read/compile *codegen-input*)
                :unless (eql ,result ,expected)
                  :do (return-from ,(car *codegen-blocks*) ,(parser-error))
                :finally (return ',object))))
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
                        (return-from ,(car *codegen-blocks*) ,(parser-error)))))))
           `(return-from ,(car *codegen-blocks*) ,(parser-error))))
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
                                  (let ((*codegen-blocks* (cons name *codegen-blocks*)))
                                    (with-fresh-stack (codegen body)))))
                      (codegen `(parser/call ,name . ,args)))
             `((lambda ,lambda-list ,(codegen body)) . ,args))))
      ((parser/call name &rest args)
       (with-gensyms (result errorp)
         `(multiple-value-bind (,result ,errorp) (,name . ,args)
            (if ,errorp
                (return-from ,(car *codegen-blocks*) ,(parser-error))
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
                (return-from ,(car *codegen-blocks*) ,(parser-error)))
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
                (return-from ,(car *codegen-blocks*) ,(parser-error))))))))))

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
