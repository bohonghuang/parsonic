(in-package #:parsonic)

(defun list->conses-1 (form)
  (destructuring-ecase form
    ((parser/list &rest args)
     (if args
         `(parser/cons ,(car args) ,(list->conses-1 `(parser/list . ,(cdr args))))
         `(parser/constantly nil)))))

(defun conses->list-1 (form)
  (destructuring-ecase form
    ((parser/cons car cdr)
     `(parser/list ,car . ,(cdr (conses->list-1 cdr))))
    ((parser/constantly null)
     (assert (null null))
     `(parser/list))))

(defun ors->or-1 (form)
  (destructuring-case form
    ((parser/or &rest parsers)
     `(parser/or
       . ,(loop :for parser :in parsers
                :for form := (ors->or parser)
                :if (eq (car form) 'parser/or)
                  :append (cdr form)
                :else
                  :collect form)))
    ((t &rest args) (declare (ignore args)) form)))

(defun cse-extract-prefix-merge-branches (branches)
  (loop :for signature :in (delete-duplicates (mapcar #'car branches) :test #'equal)
        :collect (cons signature (let ((branches (mapcar #'cdr (remove signature branches :key #'car :test-not #'equal))))
                                   (if (= (length branches) 1)
                                       (first branches)
                                       (cons `(parser/or . ,(delete-duplicates (mapcar #'car branches) :test #'equal))
                                             `(parser/or . ,(delete-duplicates (mapcar #'cdr branches) :test #'equal))))))))

(defun cse-extract-prefix-fail-p (branches)
  (and (= (length branches) 1) (eq (car (first branches)) t)))

(defvar *cse-var*)
(defvar *cse-units*)

(defun cse-extract-prefix (form)
  (macrolet ((ensure-success (results)
               (once-only (results)
                 `(if (cse-extract-prefix-fail-p ,results)
                      (return-from cse-extract-prefix (list (cons t form)))
                      ,results))))
    (destructuring-ecase form
      (((parser/satisfies parser/eql parser/constantly parser/call) &rest args)
       (declare (ignore args))
       ;; (list (list* t form '(parser/or)))
       nil
       )
      ((parser/or &rest branches)
       (let ((subbranches-list (mapcar #'cse-extract-prefix branches)))
         (cse-extract-prefix-merge-branches
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
       (loop :for (signature . (then . else)) :in (cse-extract-prefix car)
             :collect (list* signature `(parser/cons ,then ,cdr) `(parser/cons ,else ,cdr))))
      ((parser/rep parser from to)
       (if (integerp from)
           (loop :for (signature . (then . else)) :in (cse-extract-prefix parser)
                 :collect (list* signature
                                 `(parser/cons ,then (parser/rep ,parser ,(max (1- from) 0) (1- ,to)))
                                 (let ((else `(parser/cons ,else (parser/rep ,parser ,(max (1- from) 0) (1- ,to)))))
                                   (if (plusp from) else `(parser/or ,else (parser/constantly nil))))))
           nil
           ;; (list (list* t form '(parser/or)))
           ))
      ((parser/unit signature body)
       (destructuring-bind (name lambda-list) signature
         (let ((results (cons
                         (list* signature `(parser/constantly ,*cse-var*) `(parser/or))
                         (loop :for (branch-signature . (then . else)) :in (cse-extract-prefix body)
                               :collect (list* branch-signature
                                               `(parser/unit ((+ ,branch-signature ,name) ,lambda-list) ,then)
                                               `(parser/unit ((- ,branch-signature ,name) ,lambda-list) ,else))))))
           (push (cons form results) (gethash signature *cse-units*))
           results)))
      ((parser/let name bindings body)
       (if name
           nil
           ;; (list (list* t form '(parser/or)))
           (loop :for (signature . (then . else)) :in (cse-extract-prefix body)
                 :collect (list* signature `(parser/let ,name ,bindings ,then) `(parser/let ,name ,bindings ,else)))))
      ((parser/apply function parser)
       (loop :for (signature . (then . else)) :in (cse-extract-prefix parser)
             :collect (list* signature `(parser/apply ,function ,then)
                             (if (equal else '(parser/or)) '(parser/or) `(parser/apply ,function ,else)))))
      ((parser/list &rest args)
       (declare (ignore args))
       (loop :for (signature . (then . else)) :in (cse-extract-prefix (list->conses-1 form))
             :collect (list* signature (conses->list-1 then) (if (equal else '(parser/or)) '(parser/or) (conses->list-1 else)))))
      (((parser/funcall parser/filter) function &rest parsers)
       (loop :for (signature . ((apply1 f1 (list1 . ops1)) . else)) :in (cse-extract-prefix `(parser/apply ,function (parser/list . ,parsers)))
             :for (apply2 f2 (list2 . ops2)) := else
             :do (assert (eq apply1 'parser/apply)) (assert (eq list1 'parser/list))
             :collect (list* signature
                             `(,(car form) ,f1 . ,ops1)
                             (if (equal else '(parser/or))
                                 '(parser/or)
                                 (progn
                                   (assert (eq apply2 'parser/apply))
                                   (assert (eq list2 'parser/list))
                                   `(,(car form) ,f2 . ,ops2))))))
      ((parser/cut parser)
       (loop :for (signature . (then . else)) :in (cse-extract-prefix parser)
             :collect (list* signature `(parser/apply ,then) `(parser/cut ,else)))))))

(defparameter *cse-threshold* 2)

(defun or->cse (form)
  (if (consp form)
      (destructuring-case form
        ((parser/or &rest args)
         (if (>= (length args) *cse-threshold*)
             (with-gensyms (*cse-var*)
               (let ((*cse-units* (make-hash-table :test #'equal)))
                 (if-let ((units (remove 1 (cse-extract-prefix form) :key (compose #'length (rcurry #'gethash *cse-units*) #'car))))
                   (destructuring-bind (signature . (then . else))
                       (first (sort units #'> :key (compose #'length #'cdr #'first (rcurry #'gethash *cse-units*) #'car)))
                     `(parser/or
                       (parser/funcall
                        (lambda (,*cse-var*) (with-codegen ,(or->cse (ors->or-1 then))))
                        ,(or->cse (car (first (gethash signature *cse-units*)))))
                       ,(or->cse (ors->or-1 else))))
                   (cons (car form) (mapcar #'or->cse args)))))
             (cons (car form) (mapcar #'or->cse args))))
        ((t &rest args) (cons (car form) (mapcar #'or->cse args))))
      form))

(defparser test-0 ()
  '"123")

(defparser test-3 ()
  '"888")

(defparser test-1 ()
  (rep (or (test-0) (test-3)))
  '"123"
  (constantly 123))

(defparser test-2 ()
  (or (test-0) (test-3))
  '"456"
  (constantly 456))

(defparameter *optimize-passes* '(let->body satisfies->eql ors->or
                                  conses->list apply->funcall
                                  flatmap->map flatmap->let let->body
                                  lexical->unit-args remove-intermediate-units or->cse))

;; or->trie eql-list->eql*

(with-open-file (stream #P"/tmp/临时/test.txt" :direction :output :if-exists :supersede)
  (print (codegen-expand
          '(yamson::yaml-value -1))
         stream)
  nil)

(with-open-file (stream #P"/tmp/临时/test.txt" :direction :output :if-exists :supersede)
  (print *
         stream)
  nil)

(circular-tree-p *)

(parsonic::extract/compile (codegen-expand '(yamson::yaml-value -1)))

((codegen-expand '(yamson::yaml-value -1)))

(defun validate-1 (form)
  (let ((table (make-hash-table :test #'equal)))
    (labels ((recur (form)
               (if (consp form)
                   (destructuring-case form
                     (((parser/funcall parser/apply) function &rest parsers)
                      (walk-parsers-in-lambda #'recur function)
                      (mapc #'recur parsers))
                     ((parser/unit signature body)
                      (push body (gethash signature table))
                      (recur body))
                     ((t &rest args) (mapc #'recur args)))
                   form)))
      (recur form)
      table)))

(defun tree-soft-equal (a b)
  (if (symbolp a)
      (if (symbolp b)
          (or (eq a b) (and (null (symbol-package a)) (null (symbol-package b))))
          nil)
      (if (consp a)
          (if (consp b)
              (and (tree-soft-equal (car a) (car b))
                   (tree-soft-equal (cdr a) (cdr b)))
              nil)
          (equal a b))))

(defun validate-2 (table)
  (loop :for signature :being :each hash-key :of table :using (hash-value bodies)
        :do (loop :for (a b) :on bodies
                  :while b
                  :do (assert (tree-soft-equal a b)))
        :collect signature))

(defun validate (form)
  (validate-2 (validate-1 form)))

(validate (codegen-expand '(yamson::yaml-value -1)))

(extract/compile (codegen-expand '(yamson::yaml-value -1)))

(let ((*optimize-passes* '(let->body satisfies->eql ors->or
                           conses->list apply->funcall
                           flatmap->map flatmap->let let->body
                           or->trie eql-list->eql*)))
  (codegen-expand '(yamson::yaml-value -1)))

(defun parse-string (string)
  (check-type string (simple-array character (*)))
  (funcall
   (parser-lambda (input)
     (declare (type (simple-array character (*)) input)
              (optimize (speed 3) (debug 0) (safety 0)))
     (or (test-1) (test-2)))
   string))

(codegen-expand
 '(or (test-1) (test-1) (test-2) (test-2)))

(parse-string "123123")

(codegen-expand
 '(yamson::yaml-value -1)
 )
