(in-package #:parsonic)

(defun ors->or-1 (form)
  (destructuring-case form
    ((parser/or &rest parsers)
     `(parser/or
       . ,(loop :for parser :in parsers
                :for form := (ors->or-1 parser)
                :if (eq (car form) 'parser/or)
                  :append (cdr form)
                :else
                  :collect form)))
    ((t &rest args) (declare (ignore args)) form)))

(defun cse-or-delete-duplicates (form)
  (destructuring-case (ors->or-1 form)
    ((parser/or &rest args)
     (let ((args (delete-duplicates args :test #'equal)))
       (if (= (length args) 1)
           (first args)
           (cons (car form) args))))
    ((t &rest args) (declare (ignore args)) form)))

(defun cse-extract-merge-branches (branches)
  (loop :for signature :in (delete-duplicates (mapcar #'car branches) :test #'equal)
        :collect (cons signature (let ((branches (mapcar #'cdr (remove signature branches :key #'car :test-not #'equal))))
                                   (cons (cse-or-delete-duplicates `(parser/or . ,(mapcar #'car branches)))
                                         (cse-or-delete-duplicates `(parser/or . ,(mapcar #'cdr branches))))))))

(defun cse-extract-fail-p (branches)
  (and (= (length branches) 1) (eq (car (first branches)) t)))

(defvar *cse-units*)
(defvar *cse-count* 0)
(defvar *cse-vars* nil)

(defun cse-var ()
  (let ((symbol (intern (format nil "~A-~D" '#:cse-var *cse-count*) #.*package*)))
    (setf (get symbol 'cse-var) *cse-count*)
    symbol))

(defmacro cse-ensure-success (then &rest parsers)
  `(if (some (curry #'equal '(parser/or)) (list . ,parsers)) '(parser/or) ,then))

(defun cse-extract (form)
  (destructuring-ecase form
    (((parser/satisfies parser/eql parser/call) &rest args)
     (declare (ignore args)))
    (((parser/constantly) object)
     (list (list* nil `(parser/constantly ,object) '(parser/or))))
    ((parser/or &rest branches)
     (let ((subbranches-list (mapcar #'cse-extract branches)))
       (cse-extract-merge-branches
        (loop :for subbranches :in subbranches-list
              :nconc (loop :for (signature . (then . else)) :in subbranches
                           :collect (list* signature then `(parser/or . ,(loop :for branch :in branches
                                                                               :for subbranches :in subbranches-list
                                                                               :for (nil . subelse) := (assoc-value subbranches signature :test #'equal)
                                                                               :if subelse
                                                                                 :collect else
                                                                               :else
                                                                                 :collect branch))))))))
    ((parser/cons car cdr)
     (cse-extract-merge-branches
      (loop :for (signature . (then-car . else-car)) :in (cse-extract car)
            :if signature
              :collect (list* signature `(parser/cons ,then-car ,cdr) (cse-ensure-success `(parser/cons ,else-car ,cdr) else-car cdr))
            :else
              :nconc (loop :for (signature . (then-cdr . else-cdr)) :in (cse-extract cdr)
                           :collect (list* signature
                                           (cse-ensure-success `(parser/cons ,then-car ,then-cdr) then-car then-cdr)
                                           (cse-ensure-success `(parser/cons ,then-car ,else-cdr) then-car else-cdr))))))
    ((parser/rep parser from to)
     (when (integerp from)
       (cse-extract-merge-branches
        (nconc
         (loop :for (signature . (then . else)) :in (cse-extract parser)
               :collect (list* signature
                               `(parser/cons ,then (parser/rep ,parser ,(max (1- from) 0) (1- ,to)))
                               (let ((else (cse-ensure-success `(parser/cons ,else (parser/rep ,parser ,(max (1- from) 0) (1- ,to))) else)))
                                 (if (plusp from) else `(parser/or ,else (parser/constantly nil))))))
         #+nil
         (when (zerop from)
           (list (list* nil '(parser/constantly nil) '(parser/or))))))))
    ((parser/unit signature body)
     (destructuring-bind (name lambda-list) signature
       (let* ((signature (loop :for arg :in (lambda-list-arguments lambda-list)
                               :for cons := (assoc arg *cse-vars*)
                               :when cons
                                 :if (constantp (cdr cons))
                                   :collect (list arg (cdr cons)) :into args
                                 :else
                                   :return nil
                               :finally (return (list name args))))
              (results (nconc
                        (when signature (list (list* signature `(parser/constantly ,(cse-var)) `(parser/or))))
                        (loop :for (branch-signature . (then . else)) :in (cse-extract body)
                              :collect (list* branch-signature
                                              `(parser/unit ((+ ,branch-signature ,name) (,(cse-var) . ,lambda-list)) ,then)
                                              `(parser/unit ((- ,branch-signature ,name) ,lambda-list) ,else))))))
         (when signature (push (cons form results) (gethash signature *cse-units*)))
         results)))
    ((parser/let name bindings body)
     (unless name
       (loop :for (signature . (then . else)) :in (let ((*cse-vars* (nconc (nreverse
                                                                            (loop :for binding :in bindings
                                                                                  :unless (symbolp binding)
                                                                                    :collect (cons (first binding) (second binding))))
                                                                           *cse-vars*)))
                                                    (cse-extract body))
             :collect (list* signature (if (eq (first then) 'parser/constantly)
                                           (progn (assert (get (second then) 'cse-var)) then)
                                           `(parser/let ,name ,bindings ,then))
                             (cse-ensure-success `(parser/let ,name ,bindings ,else) else)))))
    ((parser/apply function parser)
     (loop :for (signature . (then . else)) :in (cse-extract parser)
           :unless signature
             :return nil
           :collect (list* signature `(parser/apply ,function ,then) (cse-ensure-success `(parser/apply ,function ,else) else))))
    ((parser/list &rest args)
     (declare (ignore args))
     (loop :for (signature . (then . else)) :in (cse-extract (list->conses-1 form))
           :collect (list* signature (conses->list-1 then) (cse-ensure-success (conses->list-1 else) else))))
    (((parser/funcall parser/filter) function &rest parsers)
     (loop :for (signature . ((apply1 f1 (list1 . ops1)) . else)) :in (cse-extract `(parser/apply ,function (parser/list . ,parsers)))
           :for (apply2 f2 (list2 . ops2)) := else
           :do (assert (eq apply1 'parser/apply)) (assert (eq list1 'parser/list))
           :collect (list* signature
                           `(,(car form) ,f1 . ,ops1)
                           (cse-ensure-success
                            (progn
                              (assert (eq apply2 'parser/apply))
                              (assert (eq list2 'parser/list))
                              `(,(car form) ,f2 . ,ops2))
                            else))))
    ((parser/cut parser)
     (loop :for (signature . (then . else)) :in (cse-extract parser)
           :collect (list* signature `(parser/cut ,then) `(parser/cut ,else))))))

(defparameter *cse-threshold* 2)

(defun or->cse (form)
  (labels ((or->cse (form)
             (typecase form
               (cons
                (destructuring-case form
                  (((parser/apply parser/funcall) function &rest args)
                   (list* (car form) (let ((*cse-count* 0)) (or->cse function)) (mapcar #'or->cse args)))
                  ((parser/or &rest args)
                   (if (>= (length args) 2)
                       (let ((*cse-units* (make-hash-table :test #'equal)))
                         (if-let ((units (loop :for unit :in (cse-extract form)
                                               :for (signature) := unit
                                               :when signature
                                                 :when (>= (length (gethash signature *cse-units*)) *cse-threshold*)
                                                   :collect unit)))
                           (destructuring-bind (signature . (then . else))
                               (first (sort units #'> :key (compose #'list-length #'cdr #'first (rcurry #'gethash *cse-units*) #'car)))
                             (ors->or-1
                              `(parser/or
                                (parser/funcall
                                 (lambda (,(cse-var))
                                   (with-codegen ,(let ((*cse-count* (1+ *cse-count*))) (or->cse then))))
                                 (parser/let nil ,(second signature) ,(or->cse (car (first (gethash signature *cse-units*))))))
                                ,(or->cse (cse-or-delete-duplicates else)))))
                           #1=(cons (car form)
                                    (let ((cse-count *cse-count*))
                                      (multiple-value-bind (branches count)
                                          (loop :for arg :in args
                                                :for *cse-count* := cse-count
                                                :collect (or->cse arg) :into branches
                                                :minimize *cse-count* :into count
                                                :finally (return (values branches (or count 0))))
                                        (setf *cse-count* (min *cse-count* count))
                                        branches)))))
                       #1#))
                  ((t &rest args) (cons (car form) (mapcar #'or->cse args)))))
               (symbol
                (when-let ((count (get form 'cse-var)))
                  (setf *cse-count* (min *cse-count* count)))
                form)
               (t form))))
    (prog1 (or->cse (remove-intermediate-units (lexical->unit-args form)))
      (assert (zerop *cse-count*)))))
