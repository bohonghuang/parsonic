(defsystem parsonic.stream
  :depends-on (#:alexandria #:parsonic #:buffered-streams)
  :serial t
  :pathname "src/"
  :components ((:module "compile"
                :components ((:file "stream")))))
