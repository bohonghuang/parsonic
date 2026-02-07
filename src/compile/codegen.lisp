(in-package #:parsonic)

(defvar *codegen-input*)
(defvar *codegen-blocks*)
(defvar *codegen-cons*)
(defvar *codegen-make-list*)
(defvar *codegen-labels*)

(defparameter *pseudo-parser-object-p* t)

(defun function-identity-p (form)
  (destructuring-case form
    ((lambda lambda-list &rest body) (equal lambda-list body))))

(define-constant +codegen-parse-error+ 'parse-failure)

(defun codegen-parse-error (&optional (info (input-position/compile *codegen-input*)))
  `(values +codegen-parse-error+ ,info))

(defun codegen-parse-error-p (result)
  `(eq ,result +codegen-parse-error+))

(defun codegen (form)
  (flet ((ignore-results-p ()
           (let ((*codegen-ignore-results-p* nil))
             (declare (special *codegen-ignore-results-p*))
             (funcall *codegen-make-list*)
             *codegen-ignore-results-p*))
         (ignore-results ()
           (let ((make-list *codegen-make-list*))
             (lambda (&optional size)
               (declare (special *codegen-ignore-results-p*))
               (unless size
                 (let ((make-list (funcall make-list)))
                   (when (boundp '*codegen-ignore-results-p*)
                     (setf *codegen-ignore-results-p* make-list))
                   make-list))))))
    (destructuring-ecase form
      ((parser/satisfies predicate)
       (if (equal predicate '(constantly nil))
           `(return-from ,(car *codegen-blocks*) ,(codegen-parse-error))
           (with-gensyms (result position)
             `(let ((,position ,(input-position/compile *codegen-input*))
                    (,result ,(input-read/compile *codegen-input*)))
                (if (funcall ,predicate ,result)
                    ,(unless (ignore-results-p) result)
                    (return-from ,(car *codegen-blocks*) ,(codegen-parse-error position)))))))
      ((parser/eql object)
       (with-gensyms (result position)
         `(let ((,position ,(input-position/compile *codegen-input*))
                (,result ,(input-read/compile *codegen-input*)))
            (if (eql ,result ,object)
                ,(unless (ignore-results-p) object)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error position))))))
      ((parser/eql* object)
       (with-gensyms (expected result position)
         `(loop :for ,expected :across ,(if (every #'characterp object) (coerce object 'simple-string) (coerce object 'simple-vector))
                :for ,position := ,(input-position/compile *codegen-input*)
                :for ,result := ,(input-read/compile *codegen-input*)
                :unless (eql ,result ,expected)
                  :do (return-from ,(car *codegen-blocks*) ,(codegen-parse-error position))
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
           (with-gensyms (position block-outer)
             `(let ((,position ,(input-position/compile *codegen-input*)))
                (block ,block-outer
                  ,@(loop :for parser :in parsers
                          :for block := (gensym (string '#:block))
                          :nconc (let ((*codegen-blocks* (cons block *codegen-blocks*)))
                                   `((block ,block
                                       (return-from ,block-outer ,(codegen parser)))
                                     ,(setf (input-position/compile *codegen-input*) position))))
                  (return-from ,(car *codegen-blocks*) ,(codegen-parse-error)))))
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
                          :finally (return `(let ,vars ,@body ,(apply function vars))))))
             (let ((function `(lambda ,(remove nil lambda-list) . ,body)))
               (if (function-identity-p function)
                   (body (curry #'list 'progn))
                   (with-fresh-stack (body (curry #'list function))))))))
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
      ((parser/apply function parser)
       (with-gensyms (arg)
         (codegen `(parser/funcall (lambda (,arg) (apply ,function ,arg)) ,parser))))
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
       (with-gensyms (result error-info)
         `(multiple-value-bind (,result ,error-info) (,name . ,args)
            (if ,(codegen-parse-error-p result)
                (return-from ,(car *codegen-blocks*) (values ,result ,error-info))
                ,result))))
      ((parser/constantly object) object)
      ((parser/rep parser from to)
       (with-gensyms (block block-outer counter position cons-root cons)
         (if-let ((list (funcall *codegen-make-list* 0))
                  (make-list (funcall *codegen-make-list* 1)))
           `(let ((,cons-root ,make-list)
                  (,counter 0)
                  (,position ,(input-position/compile *codegen-input*)))
              (declare (type non-negative-fixnum ,counter))
              (block ,block-outer
                (loop :named ,block
                      :initially (setf (cdr ,cons-root) nil)
                      :for ,cons := ,cons-root :then (cdr ,cons)
                      :repeat ,to
                      :do (setf (cdr ,cons) ,(funcall
                                              *codegen-cons*
                                              (with-nested-stack
                                                (let ((*codegen-blocks* (cons block *codegen-blocks*)))
                                                  (codegen parser)))
                                              nil)
                                ,position ,(input-position/compile *codegen-input*)
                                ,counter (1+ ,counter))
                      :finally (return-from ,block-outer))
                ,(setf (input-position/compile *codegen-input*) position))
              (setf ,list (cdr ,cons-root))
              (unless (>= ,counter ,from)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error)))
              ,list)
           `(let ((,counter 0)
                  (,position ,(input-position/compile *codegen-input*)))
              (declare (type non-negative-fixnum ,counter))
              (block ,block-outer
                (loop :named ,block
                      :repeat ,to
                      :do ,(let ((*codegen-blocks* (cons block *codegen-blocks*)))
                             (codegen parser))
                          (setf ,position ,(input-position/compile *codegen-input*)
                                ,counter (1+ ,counter))
                      :finally (return-from ,block-outer))
                ,(setf (input-position/compile *codegen-input*) position))
              (unless (>= ,counter ,from)
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error)))))))
      ((parser/case &rest branches)
       (with-gensyms (position block block-outer)
         (flet ((body ()
                  `(case ,(input-read/compile *codegen-input*)
                     ,@(loop :for (key parser) :in branches
                             :unless (eq key t) :collect `(,key ,(codegen parser)))
                     (t (return-from ,(car *codegen-blocks*) ,(codegen-parse-error position))))))
           `(let ((,position ,(input-position/compile *codegen-input*)))
              ,(if-let ((default (car (assoc-value branches t))))
                 `(block ,block-outer
                    (block ,block
                      (return-from ,block-outer
                        ,(let ((*codegen-blocks* (cons block *codegen-blocks*))) (body))))
                    ,(setf (input-position/compile *codegen-input*) position)
                    ,(codegen default))
                 (body))))))
      ((parser/cut parser)
       (let ((*codegen-blocks* (cons (lastcar *codegen-blocks*) *codegen-blocks*)))
         (codegen parser))))))
