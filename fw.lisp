(in-package #:com.liutos.fw)

(defparameter *handler* nil)

(defparameter *routers* nil)

(defmacro define-handler (method path name parameters &body body)
  (let ((handler-name (intern
                       (format nil "HANDLER/~A" (symbol-name name)))))
    `(progn
       (defun ,name ,parameters
         ,@body)
       (defun ,handler-name ,parameters
         (respond
          (,name ,@parameters)))
       (push-router ,method ,path #',handler-name))))

(defun handle-not-found (env)
  (let ((request-method (getf env :request-method))
        (request-uri (getf env :request-uri)))
    (list
     404
     '(:content-type "text/plain")
     (list (format nil "NOT FOUND: ~A ~A" request-method request-uri)))))

(defun handle-env (env)
  (let ((path-info (getf env :path-info))
        (request-method (getf env :request-method)))
    (dolist (x *routers*)
      (destructuring-bind (method path handler) x
        (when (eq method request-method)
          (cond ((and (stringp path)
                      (string= path path-info))
                 (return-from handle-env (funcall handler env)))
                ((and (listp path)
                      (cl-ppcre:scan path path-info))
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
