(asdf:defsystem #:fw
  :description "It may be a web framework for Common Lisp."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-json
               #:cl-ppcre
               #:clack)
  :serial t
  :components ((:file "package")
               (:file "fw")))
