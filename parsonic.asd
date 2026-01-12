(defsystem parsonic
  :version "0.1.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Fast parser combinators and compiler for Common Lisp."
  :homepage "https://github.com/bohonghuang/parsonic"
  :bug-tracker "https://github.com/bohonghuang/parsonic/issues"
  :source-control (:git "https://github.com/bohonghuang/parsonic.git")
  :depends-on (#:alexandria)
  :components ((:module "src"
                :components ((:file "package")
                             (:module "eval"
                              :components ((:file "input")
                                           (:file "impl" :depends-on ("input")))
                              :depends-on ("package"))
                             (:file "expand" :depends-on ("package" "eval"))
                             (:module "compile"
                              :components ((:file "input")
                                           (:file "expand")
                                           (:file "trie")
                                           (:file "optimize" :depends-on ("trie"))
                                           (:file "codegen" :depends-on ("input" "expand" "optimize"))
                                           (:file "pool")
                                           (:file "extract")
                                           (:file "macro" :depends-on ("input" "expand" "codegen" "pool" "extract")))
                              :depends-on ("package" "expand" "eval"))
                             (:file "macro" :depends-on ("package" "expand" "compile"))
                             (:module "sugar"
                              :components ((:file "quote")
                                           (:file "map")
                                           (:file "lisp" :depends-on ("map"))
                                           (:file "predef" :depends-on ("lisp")))
                              :depends-on ("package" "expand" "macro"))))))
