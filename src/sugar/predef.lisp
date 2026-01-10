(in-package #:parseq)

(defparser opt (parser)
  (or parser (constantly nil)))

(defparser repsep (rep sep &optional (from 0) (to most-positive-fixnum))
  (or (cons rep (rep (progn sep rep) (max (1- from) 0) (1- to))) (rep (or) from)))

(defconstant eof +input-eof+)

(defparser eof ()
  (progn (eql eof) (constantly nil)))
