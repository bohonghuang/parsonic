(in-package #:parsonic)

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
                        . ,(loop :for (var . dimensions) :in *codegen-list-vars*
                                 :for (size) := (ensure-list dimensions)
                                 :if (plusp size)
                                   :collect `(,var (shiftf ,list (cdr (nthcdr ,(1- size) ,list)) nil))
                                 :else
                                   :collect `(,var nil)))
                       (declare (dynamic-extent ,list)))
                     `(,(loop :for (var . dimensions) :in *codegen-list-vars*
                              :for (size) := (ensure-list dimensions)
                              :if (plusp size)
                                :collect `(,var (make-list ,size))
                              :else :if (minusp size)
                                :collect `(,var ,(loop :for form := nil :then (funcall *codegen-cons* nil form)
                                                       :repeat (abs size) :finally (return form)))
                              :else
                                :collect `(,var nil))
                       (declare (dynamic-extent . ,(mapcar #'car (remove-if-not #'plusp *codegen-list-vars* :key (compose #'car #'ensure-list #'cdr)))))))
           (let* ((,errorp ',null)
                  (,result (block ,block-outer (setf ,errorp (block ,block (return-from ,block-outer ,body))))))
             ,@(loop :for (var . dimensions) :in *codegen-list-vars*
                     :collect (labels ((recur (var dimensions &aux (size (car dimensions)))
                                         (with-gensyms (cons elem rest)
                                           (if (and size (not (plusp size)))
                                               `(when ,var
                                                  (loop :for ,cons :on ,var
                                                        :for (,elem . ,rest) := ,cons
                                                        :do ,(recur elem (cdr dimensions))
                                                        :while ,rest
                                                        ,@(unless (zerop size) `(:repeat ,(1- (abs size))))
                                                        :finally (setf (cdr ,cons) ,(funcall *codegen-cons* var))))
                                               '(progn)))))
                                (recur var (ensure-list dimensions))))
             (unless (eq ,errorp ',null)
               (return-from ,(car *codegen-blocks*) ,(codegen-parse-error result)))
             ,result))))))

(defmacro with-fresh-stack (&body body)
  `(call-with-fresh-stack (lambda () . ,body)))

(defun call-with-nested-stack (body)
  (with-gensyms (block block-outer result errorp)
    (multiple-value-bind (body list-vars)
        (let ((*codegen-list-vars* nil)
              (*codegen-blocks* (cons block *codegen-blocks*)))
          (values (funcall body) *codegen-list-vars*))
      (if list-vars
          (let ((null '#:null)
                (lists (loop :for (var . dimensions) :in list-vars
                             :collect (funcall *codegen-make-list* (cons 0 (loop :for size :in (ensure-list dimensions) :collect (- (abs size))))))))
            `(let ,(loop :for (var . dimensions) :in list-vars
                         :for (size) := (ensure-list dimensions)
                         :collect `(,var ,(loop :for form := nil :then (funcall *codegen-cons* nil form)
                                                :repeat (abs size)
                                                :finally (return form))))
               (let* ((,errorp ',null)
                      (,result (block ,block-outer (setf ,errorp (block ,block (return-from ,block-outer ,body))))))
                 ,@(loop :for (var . nil) :in list-vars
                         :for list :in lists
                         :collect `(setf ,list ,(funcall *codegen-cons* var list)))
                 (unless (eq ,errorp ',null)
                   (return-from ,(car *codegen-blocks*) ,(codegen-parse-error result)))
                 ,result)))
          `(block ,block-outer (return-from ,(car *codegen-blocks*) (block ,block (return-from ,block-outer ,body))))))))

(defmacro with-nested-stack (&body body)
  `(call-with-nested-stack (lambda () . ,body)))

(defun codegen-expand (form)
  (optimize/compile (expand/compile form)))

(defun codegen (form)
  (flet ((ignore-results-p (&aux (make-list (funcall *codegen-make-list*)))
           (unless (eq make-list *codegen-make-list*)
             make-list))
         (ignore-results ()
           (let ((make-list *codegen-make-list*))
             (lambda (&optional size)
               (unless size (funcall make-list))))))
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
       (with-gensyms (result errorp)
         `(multiple-value-bind (,result ,errorp) (,name . ,args)
            (if ,errorp
                (return-from ,(car *codegen-blocks*) ,(codegen-parse-error))
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
