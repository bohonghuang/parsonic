(in-package #:parseq)

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
    ((parser/rep parser &optional (from 0) (to most-positive-fixnum))
     (if (integerp from)
         (extract-merge-branches
          (nconc
           (loop :for (key . op) :in (extract-prefix parser)
                 :collect (cons key `(parser/cons ,op (parser/rep ,parser ,(max (1- from) 0) (1- ,to)))))
           (when (zerop from)
             (list (cons '_ '(parser/constantly nil))))))
         (throw 'fail (list (cons '_ form)))))
    ((parser/unit signature body)
     (loop :for (key . op) :in (extract-prefix body)
           :collect (cons key `(parser/unit (,key . ,signature) ,op))))
    ((parser/let name bindings body)
     (if name
         (throw 'fail (list (cons '_ form)))
         (loop :for (key . op) :in (extract-prefix body)
               :collect (cons key `(parser/let ,name ,bindings ,op)))))
    ((parser/funcall function parser)
     (loop :for (key . op) :in (extract-prefix parser)
           :collect (cons key `(parser/funcall ,function ,op))))
    ((parser/constantly object)
     (declare (ignore object))
     (list (cons '_ form)))))

(defparameter *branches-trie-threshold* 4)

(defun or->trie (form)
  (if (consp form)
      (destructuring-case form
        ((parser/or &rest args)
         (if (>= (length args) *branches-trie-threshold*)
             `(parser/case
               . ,(loop :for (key . parser) :in (extract-prefix form)
                        :collect (list key (or->trie parser))))
             (cons (car form) (mapcar #'or->trie args))))
        ((t &rest args) (cons (car form) (mapcar #'or->trie args))))
      form))
