(defpackage :hunwork
  (:use :cl :hunchentoot :cl-who)
  (:export :start-server		;Functions
	   :stop-server
	   :get-all-post-paras
	   :print-user-defined-dispatcher
	   :get-request-uri
	   :page-redirect
	   :quit-session*
	   :define-regex-dispatcher	;Macros
	   :with-get-parameter
	   :with-post-parameter
	   :with-login-let
	   :with-session-start
	   :define-static-dispatcher
	   :define-folder-dispatcher))

(in-package :hunwork)

;; (let ((acceptor nil))
;;   (defun start-acceptor (&optional (port 8080) (log-p nil) (address "localhost"))
;;     (unless acceptor
;;       (setf acceptor (make-instance 'easy-acceptor
;; 				    :address address
;; 				    :port port
;; 				    :access-log-destination log-p
;; 				    :message-log-destination nil)))
;;     (start acceptor))
;;   (defun stop-acceptor ()
;;     (stop acceptor)
;;     (setf acceptor nil)))

(defvar *acceptor* nil
  "The variable stores the acceptor used in the current web server process. The previous version's START-ACCEPTOR and STOP-ACCEPTOR function operates on a lexical variable defined in a let special-form which is hard to modify when testing code.")
(defun start-acceptor (&optional (port 8080) (log-p nil) (address "localhost")
		       (msg-log-p nil))
  (unless *acceptor*
    (setf *acceptor* (make-instance 'easy-acceptor
				    :address address
				    :port port
				    :access-log-destination log-p
				    :message-log-destination msg-log-p))) ;I try to prevent the Hunchentoot from generating the warnings when a unidentified session ID.
  (start *acceptor*))
(defun stop-acceptor ()
  (stop *acceptor*)
  (setf *acceptor* nil))

(defun start-server (&optional (port 8080) (log-p nil) (address "localhost")
		     (msg-log-p nil))
  (setf *default-content-type* "text/html; charset=utf-8")
  (setf *hunchentoot-default-external-format*
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf *show-lisp-errors-p* t)
  (start-acceptor port log-p address msg-log-p))

(defun stop-server ()
  (stop-acceptor))

(let ((hs (make-hash-table)))
  (defun add-regex-dispatcher (regex handler)
    "Bind the regex url REGEX to the HANDLER. If the HANDLER has been registered but the REGEX is different with the old one, bind the new REGEX to the HANDLER, too."
    (multiple-value-bind (v f)
	(gethash handler hs)
      (when (or (null f)
		(string/= v regex))
	(setf (gethash handler hs) regex)
	(push (create-regex-dispatcher regex handler)
	      *dispatch-table*))))
  (defun print-user-defined-dispatcher ()
    "Print all the user-defined regex-dispatcher by means of function ADD-REGEX-DISPATCHER."
    (maphash #'(lambda (k v)
		 (format t "~A: ~A~%" k v))
	     hs)))

(defun convert-fn-name->url (fn-name)
  "Convert the FN-NAME to a legal URL for a web page."
  (with-output-to-string (stream)
    (princ "^/" stream)
    (dotimes (i (length fn-name))
      (if (member (char fn-name i) '(#\- #\.) :test #'char=)
	  (princ #\\ stream))
      (princ (char fn-name i) stream))
    (princ "\\.html$" stream)))

(defmacro define-regex-dispatcher (name regex &body body)
  "Define the regex-dispatcher conveniently and register it automatically. If the REGEX is NIL, then the URL for the disposed web page is generated by a function named CONVERT-FN-NAME->URL. If the REGEX is a list of regular expression, set the function NAME as the dispatcher for each one."
  `(progn
     (defun ,name ()
       ,@body)
     ,(cond ((null regex)
	     `(add-regex-dispatcher ,(convert-fn-name->url
				      (format nil "~(~A~)" name)) ',name))
	    ((consp regex)
	     `(mapcar #'(lambda (re)
			  (add-regex-dispatcher re ',name))
		      ',regex))
	    (t `(add-regex-dispatcher ,regex ',name)))
     t))

(defmacro define-static-dispatcher (uri path)
  `(progn
     (push (create-static-file-dispatcher-and-handler ,uri ,path)
	   *dispatch-table*)
     t))

(defun get-all-post-paras ()
  (post-parameters*))

(defmacro with-post-parameter (vars &body body)
  "Bind the variables in the VARS for the corresponding values of the post parameters attached to the same symbol as each variable. After binding, evaluate the expression in BODY."
  `(let ,(mapcar #'(lambda (var)
		     `(,var (post-parameter ,(format nil "~(~A~)" var))))
		 vars)
     ,@body))

(defmacro with-get-parameter (vars &body body)
  "Acts like the macro WITH-POST-PARAMETER but this one is used for getting the parameters passed through the GET method."
  `(let ,(mapcar #'(lambda (var)
		     `(,var (get-parameter ,(format nil "~(~A~)" var))))
		 vars)
     ,@body))

(defun get-request-uri ()
  (request-uri*))

(defmacro with-login-let (vars &body body)
  "Ensure the HTTP request is send from a authenticated user by checking whether the symbol *session* has been bound. If bound, get some values from the session and bind them to the variables with same name. If not bound, the user will be guided to the index page."
  `(if *session*
       (let ,(mapcar #'(lambda (var) `(,var (session-value ',var))) vars)
	 ,@body)
       (with-html-output-to-string (*standard-output*)
	 (:html
	  (:body
	   (:p "You must login at first. Please go to the index page for login.")
	   (:a :href "/index.html"
	       "Click here to return"))))))

(defmacro with-session-start (session-values &body body)
  "Use Hunchentoot's START-SESSION function for using the session in communication. The tuple in argument SESSION-VALUES contains the symbol would be set in session and the corresponding value. Then evaluate the expression in BODY."
  `(progn
     (reset-session-secret)
     (setf ,@(mapcan #'(lambda (binding)
			 (destructuring-bind (symbol value) binding
			   `((session-value ',symbol) ,value)))
		     session-values))
     (start-session)
     ,@body))

(defun page-redirect (url &key (add-session-id nil))
  (redirect url :add-session-id add-session-id))

(defun quit-session* ()
  (remove-session *session*))

(defmacro define-folder-dispatcher (uri-prefix base-path)
  `(progn
     (push (create-folder-dispatcher-and-handler ,uri-prefix ,base-path)
	   *dispatch-table*)
     t))