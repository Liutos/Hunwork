(in-package #:com.liutos.fw-user)

(defparameter *program*
  '((:id 1
     :type "statement"
     :source "a = b;")
    (:id 2
     :type "expression"
     :source "b + 1")
    (:id 3
     :type "expression"
     :source "1")))

(defun object-get (id)
  (dolist (x *program*)
    (when (= (getf x :id) id)
      (return-from object-get x))))

(defun handler/get (env)
  (let ((request-uri (getf env :request-uri)))
    (list
     200
     '(:content-type "text/plain")
     (let ((id (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" request-uri))))
       (let ((obj (object-get id)))
         (if (null obj)
             (list (json:encode-json-to-string obj))
             (list (json:encode-json-plist-to-string obj))))))))

(push (list
       :get
       (cl-ppcre:parse-string "/[0-9]+")
       #'handler/get)
      com.liutos.fw::*routers*)

(defun handler/delete (env)
  (let ((request-uri (getf env :request-uri)))
    (list
     200
     '(:content-type "text/plain")
     (let ((id (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" request-uri))))
       (setf *program*
             (remove-if #'(lambda (x)
                            (= id (getf x :id)))
                        *program*))
       (list (format nil "object with id ~D deleted" id))))))

(push (list
       :delete
       (cl-ppcre:parse-string "/[0-9]+")
       #'handler/delete)
      com.liutos.fw::*routers*)
