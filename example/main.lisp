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

(define-handler handler/get (env)
  (let ((request-uri (getf env :request-uri)))
    (let ((id (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" request-uri))))
      (let ((obj (object-get id)))
        (if (null obj)
            (json:encode-json-to-string obj)
            (json:encode-json-plist-to-string obj))))))

(push-router :get "/[0-9]+" #'handler/get)

(define-handler handler/delete (env)
  (let ((request-uri (getf env :request-uri)))
    (let ((id (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" request-uri))))
      (setf *program*
            (remove-if #'(lambda (x)
                           (= id (getf x :id)))
                       *program*))
      (format nil "object with id ~D deleted" id))))

(push-router :delete "/[0-9]+" #'handler/delete)
