(in-package :com.liutos.fw)

(defun ver-compare (v1 v2)
  "Compare two version strings. Return 1 or 0 or -1 when `v1' is greater or equal or smaller than `v2'."
  (let ((vp1 (split "\\." v1))
        (vp2 (split "\\." v2)))
    (dotimes (i (max (length vp1) (length vp2)) 0)
      (let ((p1 (nth i vp1))
            (p2 (nth i vp2))
            n1 n2)
        (when (and (null p1) p2)
          (return-from ver-compare -1))
        (when (and (null p1) (null p2))
          (return-from ver-compare 0))
        (when (and p1 (null p2))
          (return-from ver-compare 1))
        (setf n1 (parse-integer p1)
              n2 (parse-integer p2))
        (cond ((> n1 n2)
               (return-from ver-compare 1))
              ((< n1 n2)
               (return-from ver-compare -1)))))))

(defun ver= (v1 v2) (= (ver-compare v1 v2) 0))
(defun ver< (v1 v2) (< (ver-compare v1 v2) 0))
(defun ver> (v1 v2) (> (ver-compare v1 v2) 0))
(defun ver<= (v1 v2) (<= (ver-compare v1 v2) 0))
(defun ver>= (v1 v2) (>= (ver-compare v1 v2) 0))
(defun ver/= (v1 v2) (/= (ver-compare v1 v2) 0))