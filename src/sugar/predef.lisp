(in-package #:parsonic)

(defparser opt (parser)
  (or parser (constantly nil)))

(defparser repsep (rep sep &optional (from 0) (to most-positive-fixnum))
  (or (cons rep (rep (progn sep rep) (max (1- from) 0) (1- to))) (rep (or) from)))

(defconstant eof +input-eof+)

(defparser eof ()
  (progn (eql eof) (constantly nil)))

(defparser peek (parser)
  ((lambda ()
     (let ((value #1='#:peek-fail))
       (parser
        (or ((lambda (result) (setf value result) (parser (or))) parser)
            ((lambda () (if (eq value #1#) (parser (or)) (parser (constantly value)))))))))))
