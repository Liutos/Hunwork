(asdf:defsystem #:fw
  :description "It may be a web framework for Common Lisp."
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-json
               #:cl-ppcre
               #:hunchentoot
               #:md5
               #:metabang-bind)
  :serial t
  :components ((:file "package")
               (:file "ll")
               (:file "fw")))
