(defpackage :hunwork
  (:use :cl :hunchentoot)
  (:export :start-server
	   :stop-server
	   :define-regex-dispatcher
	   :get-all-post-paras
	   :with-post-parameter
	   :print-user-defined-dispatcher))

(in-package :hunwork)

(let ((acceptor nil))
  (defun start-acceptor (&optional (port 8080) (log-p nil))
    (unless acceptor
      (setf acceptor (make-instance 'easy-acceptor
				    :port port
				    :access-log-destination log-p)))
    (start acceptor))
  (defun stop-acceptor ()
    (stop acceptor)
    (setf acceptor nil)))

(defun start-server (&optional (port 8080))
  (setf *default-content-type* "text/html; charset=utf-8")
  (setf *hunchentoot-default-external-format*
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf *show-lisp-errors-p* t)
  (start-acceptor port))

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

(defun get-all-post-paras ()
  (post-parameters*))

(defmacro with-post-parameter (vars &body body)
  `(let ,(mapcar #'(lambda (var)
		     `(,var (post-parameter ,(format nil "~(~A~)" var))))
		 vars)
     ,@body))