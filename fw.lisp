(in-package #:com.liutos.fw)

(defparameter *handler* nil)

(defparameter *routers* nil)

(defun handle-not-found (env)
  (let ((request-method (getf env :request-method))
        (request-uri (getf env :request-uri)))
    (list
     404
     '(:content-type "text/plain")
     (list (format nil "NOT FOUND: ~A ~A" request-method request-uri)))))

(defun handle-env (env)
  (let ((request-method (getf env :request-method))
        (request-uri (getf env :request-uri)))
    (dolist (x *routers*)
      (destructuring-bind (method path handler) x
        (when (eq method request-method)
          (cond ((and (stringp path)
                      (string= path request-uri))
                 (return-from handle-env (funcall handler env)))
                ((and (listp path)
                      (cl-ppcre:scan path request-uri))
                 (return-from handle-env (funcall handler env)))))))
    (handle-not-found env)))

(defun start-app ()
  (setf *handler*
        (clackup
         (lambda (env)
           (handle-env env))))
  nil)

(defun stop-app ()
  (stop *handler*)
  (setf *handler* nil))
