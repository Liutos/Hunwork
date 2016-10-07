(asdf:defsystem #:fw-user
  :description "An example web app powered by Hunwork"
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (#:fw)
  :serial t
  :components ((:file "package")
               (:file "main")))
