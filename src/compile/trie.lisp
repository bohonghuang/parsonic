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
     `(parser/list))
    ((parser/or &rest args)
     (if args
         `(parser/list . ,(apply #'mapcar (lambda (&rest args) `(parser/or . ,args)) (mapcar (compose #'cdr #'conses->list-1) args)))
         `(parser/or)))))

(defun trie-extract-merge-branches (branches)
  (loop :for key :in (delete-duplicates (mapcar #'car branches) :test #'equal)
        :collect (cons key (let ((ops (mapcar #'cdr (remove key branches :key #'car :test-not #'eql))))
                             (if (> (length ops) 1)
                                 `(parser/or . ,ops)
                                 (first ops))))))

(defun trie-extract-fail-p (branches)
  (and (= (length branches) 1) (eq (car (first branches)) t)))

(defmacro trie-ensure-success (results)
  (once-only (results)
    `(if (trie-extract-fail-p ,results)
         (return-from trie-extract (list (cons t form)))
         ,results)))

(defun trie-extract (form)
  (destructuring-ecase form
    ((parser/satisfies function)
     (declare (ignore function))
     (list (cons t form)))
    ((parser/eql object)
     (list (cons (list object) `(parser/constantly ,object))))
    ((parser/or &rest branches)
     (loop :with results := nil
           :for branch :in branches
           :do (loop :for (key . op) :in (trie-extract branch)
                     :do (nconcf (assoc-value results key :test #'equal) (list op)))
           :finally
              (loop :for result :in results
                    :if (> (length (cdr result)) 1)
                      :do (setf (cdr result) `(parser/or . ,(cdr result)))
                    :else
                      :do (setf (cdr result) (first (cdr result))))
              (return results)))
    ((parser/cons car cdr)
     (trie-extract-merge-branches
      (loop :for (key . op-car) :in (trie-ensure-success (trie-extract car))
            :if key
              :collect (cons key `(parser/cons ,op-car ,cdr))
            :else
              :nconc (loop :for (key . op-cdr) :in (trie-extract cdr)
                           :collect (cons key `(parser/cons ,op-car ,op-cdr))))))
    ((parser/rep parser from to)
     (if (integerp from)
         (trie-extract-merge-branches
          (nconc
           (loop :for (key . op) :in (trie-ensure-success (trie-extract parser))
                 :do (assert key)
                 :collect (cons key `(parser/cons ,op (parser/rep ,parser ,(max (1- from) 0) (1- ,to)))))
           #-nil
           (when (zerop from)
             (list (cons nil '(parser/constantly nil))))))
         (list (cons t form))))
    ((parser/unit signature body)
     (destructuring-bind (name lambda-list) signature
       (loop :for (key . op) :in (trie-ensure-success (trie-extract body))
             :collect (cons key `(parser/unit ((,key ,name) ,lambda-list) ,op)))))
    ((parser/let name bindings body)
     (if name
         (list (cons t form))
         (loop :for (key . op) :in (trie-extract body)
               :collect (cons key `(parser/let ,name ,bindings ,op)))))
    ((parser/apply function parser)
     (loop :for (key . op) :in (trie-extract parser)
           :unless key
             :return (list (cons t form))
           :collect (cons key `(parser/apply ,function ,op))))
    ((parser/list &rest args)
     (declare (ignore args))
     (loop :for (key . op) :in (trie-extract (list->conses-1 form))
           :collect (cons key (conses->list-1 op))))
    ((parser/funcall function &rest parsers)
     (loop :for (key . (apply f (list . ops))) :in (trie-extract `(parser/apply ,function (parser/list . ,parsers)))
           :do (assert (eq apply 'parser/apply)) (assert (eq list 'parser/list))
           :collect (cons key `(parser/funcall ,f . ,ops))))
    ((parser/filter function &rest parsers)
     (loop :for (key . (list . ops)) :in (trie-extract `(parser/list . ,parsers))
           :do (assert (eq list 'parser/list))
           :collect (cons key `(parser/filter ,function . ,ops))))
    ((parser/constantly object)
     (declare (ignore object))
     (list (cons nil form)))
    ((parser/cut parser)
     (loop :for (key . op) :in (trie-extract parser)
           :collect (cons key `(parser/cut ,op))))
    ((parser/call name &rest args)
     (declare (ignore name args))
     (list (cons t form)))))

(defparameter *trie-threshold* 4)

(defun or->trie (form)
  (labels ((or->trie (form)
             (if (consp form)
                 (destructuring-case form
                   ((parser/or &rest args)
                    (if (>= (length args) 2)
                        (let ((branches (trie-extract form)))
                          (if (>= (length branches) *trie-threshold*)
                              `(parser/case
                                . ,(loop :for (key . parser) :in branches
                                         :for branch := (or->trie parser)
                                         :if key
                                           :collect (cons key branch) :into key-branches
                                         :else
                                           :collect (cons t branch) :into t-branches
                                         :finally
                                            (return
                                              (loop :for (key . branch) :in (trie-extract-merge-branches
                                                                             (nconc key-branches t-branches))
                                                    :collect (list key branch)))))
                              (cons (car form) (mapcar #'or->trie args))))
                        (cons (car form) (mapcar #'or->trie args))))
                   ((t &rest args) (cons (car form) (mapcar #'or->trie args))))
                 form)))
    (or->trie form)))
