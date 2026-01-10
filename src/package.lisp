(defpackage parseq
  (:use #:cl #:alexandria)
  (:export
   #:defparser
   #:parser
   #:parser-run
   #:parser-lambda
   #:for
   #:opt
   #:repsep
   #:rep
   #:eof))

(in-package #:parseq)
