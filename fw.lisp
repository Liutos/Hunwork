(in-package #:com.liutos.fw)

;;; Utilities
(defmacro define-func (name args &body body)
  `(defun ,name ,args
     (symbol-macrolet ((__func__ ',name))
       ,@body)))

(defmacro setf-default (place new-value)
  `(when (null ,place)
     (setf ,place ,new-value)))

;;; Variable declarations
(defvar *application*)
(defvar *console-output* *standard-output*)
(defvar *raw-routes* '())
(defvar *routes* '())
(defvar *update-routes-p* t)

;;; Data type definitions
(defclass route-rule ()
  ((handler :initarg :handler
            :reader route-rule-handler)
   (path :initarg :path
         :reader route-rule-path)
   (regex :initarg :regex
          :reader route-rule-regex)
   (verb :initarg :verb
         :reader route-rule-verb)))

(defconstant +PORT+ 4242)

(defclass application (acceptor)
  ((static-path :accessor application-static-path
                :initarg :static-path)
   (static-root :accessor application-static-root
                :initarg :static-root)))

(defun dispatch-handler (request)
  (dolist (rule *routes*)
    (etypecase rule
      (route-rule
       (with-slots (handler regex verb) rule
         (when (and (or (eq verb :*) (eq (request-method request) verb)) ; No verb requirement or match
                    (if (stringp regex)
                        (string= regex (script-name request))
                        (scan regex (script-name request))))
           (if (symbolp handler)
               (return (symbol-function handler))
               (return handler))))))))

(defmethod acceptor-dispatch-request ((acceptor application) request)
  (let ((handler (dispatch-handler request)))
    (if handler
        (funcall handler)
        (call-next-method))))

(defclass response ()
  ((body :initarg :body
         :reader response-body)
   (content-type :accessor response-content-type
                 :initarg :content-type)))

(defmethod initialize-instance :after ((instance response) &rest initargs)
  (declare (ignore initargs))
  (setf-default (response-content-type instance) "text/html"))

(defun make-response (body
                      &optional
                        content-type)
  (make-instance 'response
                 :body body
                 :content-type content-type))

;;; Private functions
(defun handler-name (name)
  (intern (format nil "HANDLER/~A" name)))

(defun parse-verb (args)
  (getf args :verb))

(defun parse-uri (args)
  (getf args :uri))

(defun parse-description (description)
  (etypecase description
    (cons (destructuring-bind (name . args) description
            (let* ((verb (parse-verb args))
                   (uri (parse-uri args))
                   (args (if verb
                             (append args '(:allow-other-keys t))
                             args)))
              (values name
                      (cons (handler-name name)
                            args)
                      verb
                      uri))))
    (symbol (values description
                    (handler-name description)))))

(defun parse-lambda-list (lambda-list)
  (multiple-value-bind (reqs opts)
      (split-lambda-list lambda-list)
    (values (append reqs
                    (mapcar #'(lambda (e) (if (consp e) (car e) e)) opts))
            (append reqs opts))))

(defun update-route (verb uri handler)
  (let ((key (format nil "~A ~A" verb uri)))
    (setf (gethash key *routes*) handler)))

(defmacro define-easy-handler-no-verb ((name args body) (description uri lambda-list))
  (let ((response (gensym)))
    `(progn
       ;; Unbind the previous generic function if exists
       (when (and (fboundp ',name)
                  (typep (symbol-function ',name) 'generic-function))
         (fmakunbound ',name))

       (defun ,name ,args ,@body)
       (hunchentoot:define-easy-handler ,description ,lambda-list
         (let ((,response (,name ,@args)))
           (setf (content-type*) (response-content-type ,response))
           (response-body ,response)))
       (update-route nil ,uri ',name))))

(defmacro define-easy-handler-with-verb ((name verb args body) (description uri lambda-list))
  (let ((response (gensym)))
    `(progn
       ;; Unbind the previous ordinary function
       (when (and (fboundp ',name)
                  (not (typep (symbol-function ',name) 'generic-function)))
         (fmakunbound ',name))
       ;; The first time to define the generic function
       (unless (fboundp ',name)
         (defgeneric ,name (verb ,@args)))

       (defmethod ,name ((verb (eql ,verb)) ,@args)
         (declare (ignorable verb))
         ,@body)
       (hunchentoot:define-easy-handler ,description ,lambda-list
         (let ((,response (,name (request-method*) ,@args)))
           (setf (content-type*) (response-content-type ,response))
           (response-body ,response)))
       (update-route ,verb ,uri ',name))))

(define-func debug-headers-in ()
  (loop
     :for (k . v) :in (headers-in*)
     :do (format t "[DEBUG / ~(~A~)] ~S = ~S~%" __func__ k v)))

(define-func debug-post-parameters ()
  (loop
     :for (k . v) :in (post-parameters*)
     :do (format t "[DEBUG / ~(~A~)] ~S = ~S~%" __func__ k v)))

(defmacro with-io-control (lambda-list expr)
  (let ((response (gensym)))
    `(let ,(mapcar #'(lambda (var)
                       (if (consp var)
                           `(,(first var)
                              (or (hunchentoot::compute-parameter ,(format nil "~(~A~)" (first var)) 'string :both)
                                  ,(second var)))
                           `(,var (hunchentoot::compute-parameter ,(format nil "~(~A~)" var) 'string :both))))
                   lambda-list)
       (let* ((*standard-output* *console-output*)
              (,response))
         (debug-headers-in)
         (debug-post-parameters)
         (setf ,response (progn ,expr))
         (cond ((typep ,response 'response)
                (setf (content-type*) (response-content-type ,response))
                (response-body ,response))
               (t ,response))))))

(defparameter *placeholder* ":[^/]+")

(defun parse-vars-and-uri (uri)
  (let ((names (all-matches-as-strings *placeholder* uri))
        (new-uri (regex-replace-all *placeholder* uri "([^/]+)")))
    (values (mapcar #'(lambda (name)
                        (intern (string-upcase (subseq name 1))))
                    names)
            new-uri
            (mapcar #'(lambda (name)
                        (intern (string-upcase (subseq name 1)) :keyword))
                    names))))

(defun double-valid-p (&rest args)
  "Return T if there are more than one no-null value in `args'"
  (let ((cnt 0))
    (dolist (arg args)
      (when arg
        (incf cnt))
      (when (>= cnt 2)
        (return-from double-valid-p t)))
    nil))

(defun ensure-route (verb path uri handler)
  (unless (find-if #'(lambda (rule)
                       (and (typep rule 'route-rule)
                            (eq (route-rule-verb rule) verb)
                            (equal (route-rule-regex rule) uri)
                            (eq (route-rule-handler rule) handler)))
                   *routes*)
    (push (make-instance 'route-rule
                         :handler handler
                         :path path
                         :regex (if (characterp uri) (string uri) uri)
                         :verb verb)
          *routes*)))

;;; Public functions
(defmacro define-easy-handler (description lambda-list &body body)
  (multiple-value-bind (name description verb uri)
      (parse-description description)
    (let ((args (parse-lambda-list lambda-list)))
      (if verb
          `(define-easy-handler-with-verb (,name ,verb ,args ,body) (,description ,uri ,lambda-list))
          `(define-easy-handler-no-verb (,name ,args ,body) (,description ,uri ,lambda-list))))))

(defmacro define-handler (verb path name lambda-list &body body)
  (bind (((:values args defs) (parse-lambda-list lambda-list))
         (handler (intern (format nil "~A/~A" verb name)))
         ((:values uriargs regex keys) (parse-vars-and-uri path))
         (uri (parse-string regex)))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (defun ,handler ()
         (with-io-control ,defs
           ,(if uriargs
                (let ((result (gensym)))
                  `(let (,result)
                     (do-register-groups ,uriargs
                         (,regex (script-name*))
                       (setf ,result
                             (,name ,@args
                                    ,@(mapcan #'list keys uriargs))))
                     ,result))
                `(,name ,@args))))
       (push (list ,verb ,path ',name)
             *raw-routes*)
       (when *update-routes-p*
         (ensure-route ,verb ,path ',uri ',handler)))))

(defmacro define-routes (&body body)
  `(progn
     ,@(mapcar #'(lambda (rule)
                   (destructuring-bind (verb path name) rule
                     (let* ((handler (intern (format nil "~A/~A" verb name)))
                            (regex (nth-value 1 (parse-vars-and-uri path)))
                            (uri (parse-string regex)))
                       `(ensure-route ,verb ,path ',uri ',handler))))
               body)))

(defun clear-routes ()
  (setf *routes* '()))

(defun define-folder-handler (uri-prefix base-path)
  (let* ((path (concatenate 'string "^" uri-prefix "(.+)"))
         (regex (parse-string path)))
    (flet ((handler ()
             (do-register-groups (suffix)
                 (regex (script-name*))
               (let ((path (concatenate 'string (namestring base-path) suffix)))
                 (return-from handler (handle-static-file path))))))
      (when *update-routes-p*
        (ensure-route :* path regex #'handler)))))

(defun md5 (str &key (upper-case-p nil))
  (declare (type string str))
  (let* ((sum (md5sum-string str))
         (hex (with-output-to-string (s)
                (dotimes (i (length sum))
                  (format s "~(~2,'0X~)" (aref sum i))))))
    (if upper-case-p
        (string-upcase hex)
        hex)))

(defun print-routes ()
  (dolist (rule *routes*)
    (when (typep rule 'route-rule)
      (with-slots (handler path regex verb) rule
        (format t "~A ~A => ~A~%" verb path handler)))))

(defun render (&key
                 file
                 html
                 json
                 nothing
                 plain)
  (when (double-valid-p file
                        html
                        json
                        nothing
                        plain)
    (error "Only one valid argument allowed in ~{`~(~A~)'~^, ~}"
           '(file html json nothing plain)))
  (cond (file
         (make-response (handle-static-file file)))
        (html
         (make-response html "text/html"))
        (json
         (let* ((*lisp-identifier-name-to-json* 'identity)
                (str (encode-json-to-string json)))
           (make-response str "application/json")))
        (nothing
         (make-response ""))
        (plain
         (make-response plain "text/plain"))))

(defun init (&key
               (access-log-destination *standard-output*)
               document-root
               (message-log-destination *standard-output*)
               (port +PORT+)
               static-path
               static-root)
  (make-instance 'application
                 :access-log-destination access-log-destination
                 :document-root document-root
                 :message-log-destination message-log-destination
                 :port port
                 :static-path static-path
                 :static-root static-root))

(defun update-routes (rules)
  (mapc #'(lambda (rule)
            (destructuring-bind (verb path name) rule
              (let* ((handler (intern (format nil "~A/~A" verb name)))
                     (regex (nth-value 1 (parse-vars-and-uri path)))
                     (uri (parse-string regex)))
                (ensure-route verb path uri handler))))
        rules))

(defun start (&key
                (app *application*)
                (update-routes-p nil))
  (with-slots (static-path static-root) app
    (when (and static-path static-root)
      (define-folder-handler static-path static-root)))
  (hunchentoot:start app)
  (when update-routes-p
    (update-routes *raw-routes*)))

(defun stop (&optional (app *application*))
  (hunchentoot:stop app)
  (clear-routes))
