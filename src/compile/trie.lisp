(in-package #:parsonic)

(defun extract-merge-branches (branches)
  (loop :for key :in (delete-duplicates (mapcar #'car branches))
        :collect (cons key (let ((ops (mapcar #'cdr (remove key branches :key #'car :test-not #'eql))))
                             (if (> (length ops) 1)
                                 `(parser/or . ,ops)
                                 (first ops))))))

(defun extract-prefix (form)
  (destructuring-ecase form
    ((parser/satisfies function)
     (declare (ignore function))
     (throw 'fail (list (cons '_ form))))
    ((parser/eql object)
     (list (cons object `(parser/constantly ,object))))
    ((parser/or &rest branches)
     (loop :with results := nil
           :for branch :in branches
           :do (loop :for (key . op) :in (block succ (catch 'fail (return-from succ (extract-prefix branch))) (list (cons '_ branch)))
                     :do (nconcf (assoc-value results key) (list op)))
           :finally
              (loop :for result :in results
                    :if (> (length (cdr result)) 1)
                      :do (setf (cdr result) `(parser/or . ,(cdr result)))
                    :else
                      :do (setf (cdr result) (first (cdr result))))
              (return results)))
    ((parser/cons car cdr)
     (extract-merge-branches
      (loop :for (key . op-car) :in (extract-prefix car)
            :if (and (eq key '_))
              :nconc (loop :for (key . op-cdr) :in (extract-prefix cdr)
                           :collect (cons key `(parser/cons ,op-car ,op-cdr)))
            :else
              :collect (cons key `(parser/cons ,op-car ,cdr)))))
    ((parser/rep parser from to)
     (if (integerp from)
         (extract-merge-branches
          (nconc
           (loop :for (key . op) :in (extract-prefix parser)
                 :collect (cons key `(parser/cons ,op (parser/rep ,parser ,(max (1- from) 0) (1- ,to)))))
           (when (zerop from)
             (list (cons '_ '(parser/constantly nil))))))
         (throw 'fail (list (cons '_ form)))))
    ((parser/unit signature body)
     (destructuring-bind (name lambda-list) signature
       (loop :for (key . op) :in (extract-prefix body)
             :if (eq key '_)
               :collect (cons key `(parser/unit (,name ,lambda-list) ,op))
             :else
               :collect (cons key `(parser/unit ((,key ,name) ,lambda-list) ,op)))))
    ((parser/let name bindings body)
     (if name
         (throw 'fail (list (cons '_ form)))
         (loop :for (key . op) :in (extract-prefix body)
               :collect (cons key `(parser/let ,name ,bindings ,op)))))
    ((parser/apply function parser)
     (loop :for (key . op) :in (extract-prefix parser)
           :collect (cons key `(parser/apply ,function ,op))))
    ((parser/constantly object)
     (declare (ignore object))
     (list (cons '_ form)))
    ((parser/cut parser)
     (loop :for (key . op) :in (extract-prefix parser)
           :collect (cons key `(parser/cut ,op))))
    ((parser/call name &rest args)
     (declare (ignore name args))
     (throw 'fail (list (cons '_ form))))))

(defparameter *branches-trie-threshold* 4)

(defun or->trie (form)
  (if (consp form)
      (destructuring-case form
        ((parser/or &rest args)
         (if (>= (length args) *branches-trie-threshold*)
             `(parser/case
               . ,(loop :for (key . parser) :in (extract-prefix form)
                        :collect (list key (if (equal parser form)
                                               `(parser/or . ,(mapcar #'or->trie args))
                                               (or->trie parser)))))
             (cons (car form) (mapcar #'or->trie args))))
        ((t &rest args) (cons (car form) (mapcar #'or->trie args))))
      form))
