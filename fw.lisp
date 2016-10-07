(in-package #:com.liutos.fw)

(defparameter *handler* nil)

(defparameter *routers* nil)

(defmacro define-handler (method path name parameters &body body)
  `(progn
     (defun ,name ,parameters
       (respond
        ,@body))
     (push-router ,method ,path #',name)))

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

(defun push-router (method path handler)
  (push (list
         method
         (cl-ppcre:parse-string path)
         handler)
        *routers*))

(defun respond (body
                &key
                  (code 200)
                  (content-type "text/plain"))
  (list
   code
   (list :content-type content-type)
   (list body)))

(defun start-app ()
  (setf *handler*
        (clackup
         (lambda (env)
           (handle-env env))))
  nil)

(defun stop-app ()
  (stop *handler*)
  (setf *handler* nil))
