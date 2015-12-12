(in-package :com.liutos.fw)

(defun take-while-from (lst start-pred end-pred
                        &key
                          (include-start-p t))
  (labels ((aux (lst coll startp)
             (cond ((null lst)
                    coll)
                   ((not startp)
                    (if (funcall start-pred (car lst))
                        (if include-start-p
                            (aux (cdr lst) (cons (car lst) coll) t)
                            (aux (cdr lst) coll t))
                        (aux (cdr lst) coll nil)))
                   ((and end-pred (funcall end-pred (car lst)))
                    coll)
                   (t (aux (cdr lst) (cons (car lst) coll) startp)))))
    (nreverse
     (if (null start-pred)
         (aux lst '() t)
         (aux lst '() nil)))))

(defun split-lambda-list (lambda-list)
  (flet ((end-pred (e)
           (member e lambda-list-keywords)))
    (values (take-while-from lambda-list nil #'end-pred)
            (take-while-from lambda-list (curry #'eq '&optional) #'end-pred :include-start-p nil)
            (take-while-from lambda-list (curry #'eq '&rest) #'end-pred :include-start-p nil)
            (take-while-from lambda-list (curry #'eq '&key) #'end-pred :include-start-p nil))))
