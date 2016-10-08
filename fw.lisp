(in-package #:com.liutos.fw)

(defparameter *handler* nil)

(defparameter *routers* nil)

(defun make-handler-name (name)
  (intern (format nil "HANDLER/~A" (symbol-name name))))

(defun make-getf-form (env names)
  (flet ((f (name)
           `(getf ,env ,(intern (symbol-name name) :keyword))))
    (mapcar #'f names)))

(defun make-query-alist (env)
  (let* ((query-string
          (format nil "?~A" (getf env :query-string)))
         (queries (query-parameters-of (parse-uri query-string))))
    (flet ((f (kv)
             (destructuring-bind (key . value) kv
               (cons (intern (string-upcase key) :keyword)
                     value))))
      (mapcar #'f queries))))

(defmacro define-handler (method path name parameters &body body)
  (let+ (((&values rs os)
          (parse-ordinary-lambda-list parameters))
         (env (gensym))
         (handler-name (make-handler-name name))
         (query (gensym)))
    (let ((envs (make-getf-form env rs)))
      `(progn
         (defun ,name ,parameters
           ,@body)
         (defun ,handler-name (,env)
           (declare (ignorable ,env))
           (let* ((,query (make-query-alist ,env)))
             (respond
              (,name
               ,@envs
               ,@(mapcar #'(lambda (o)
                             `(or (cdr (assoc ,(intern (symbol-name (first o)) :keyword)
                                              ,query))
                                  ,(second o)))
                         os)))))
         (push-router ,method ,path #',handler-name)))))

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
