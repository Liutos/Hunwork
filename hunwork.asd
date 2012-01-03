(in-package :cl-user)

(defpackage :hunwork-system
  (:use :cl :asdf))

(in-package :hunwork-system)

(defsystem :hunwork
  :depends-on (:hunchentoot)
  :components ((:file "hunwork")))