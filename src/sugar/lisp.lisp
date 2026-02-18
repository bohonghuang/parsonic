(in-package #:parsonic)

(defun body-parser-form (body)
  (case (length body)
    (0 '(or))
    (1 (first body))
    (t `(progn . ,body))))

(defmethod expand-expr ((op (eql 'let)) &rest args)
  (destructuring-bind (bindings &rest body) args
    (multiple-value-bind (declarations body) (body-declarations body)
      (multiple-value-bind (args ignored)
          (loop :for (name) :in bindings
                :for gensym := (gensym)
                :if name
                  :collect name :into names
                :else
                  :collect gensym :into names
                  :and :collect gensym :into ignored
                :finally (return (values names ignored)))
        (expand
         `(apply
           (lambda ,args
             (declare (ignore . ,ignored) . ,declarations)
             (parser ,(body-parser-form body)))
           (list . ,(mapcar #'second bindings))))))))

(defmethod expand-expr ((op (eql 'let*)) &rest args)
  (destructuring-bind (bindings &rest body) args
    (expand
     (loop :for form := (body-parser-form body) :then `(let (,binding) ,form)
           :for binding :in (reverse bindings)
           :finally (return form)))))

(defmethod expand-expr ((op (eql 'for)) &rest args)
  (destructuring-bind (bindings &rest body) args
    (multiple-value-bind (declarations body) (body-declarations body)
      (expand `(let ,bindings
                 (declare . ,declarations)
                 (constantly
                  ,(case (length body)
                     (0 nil)
                     (1 (first body))
                     (t `(progn . ,body)))))))))

(defmethod expand-expr ((op cons) &rest args)
  (destructuring-ecase op
    ((lambda lambda-list &rest body)
     (declare (ignore lambda-list body))
     (expand `(funcall ,op . ,args)))))

(defmacro for (bindings &body body)
  (declare (ignore bindings body)))

(defmethod expand-expr ((op (eql 'funcall)) &rest args)
  (destructuring-bind (function &rest args) args
    (expand `(apply ,function (list . ,args)))))

(defmethod expand-expr ((op (eql 'list)) &rest args)
  (loop :for body := '(parser/constantly nil) :then `(parser/cons ,(expand arg) ,body)
        :for arg :in (reverse args)
        :finally (return body)))

(defmethod expand-expr ((op (eql 'progn)) &rest args)
  (with-gensyms (var)
    (expand `(for ((nil (list . ,(butlast args)))
                   (,var ,(lastcar args)))
               ,var))))

(defmethod expand-expr ((op (eql 'and)) &rest args)
  (expand `(progn . ,args)))

(defmethod expand-expr ((op (eql 'prog1)) &rest args)
  (with-gensyms (var)
    (expand `(for ((,var ,(first args))
                   (nil (list . ,(rest args))))
               ,var))))

(defmethod expand-expr ((op (eql 'prog2)) &rest args)
  (with-gensyms (var)
    (expand `(for ((nil ,(first args))
                   (,var ,(second args))
                   (nil (list . ,(nthcdr 2 args))))
               ,var))))
