(in-package #:cl-user)

(defconstant +code+
  '(((nil	t  	t  	t)  	#\1)
    ((t  	nil	t  	t)  	#\2)
    ((nil	nil	t  	t)  	#\3)
    ((t  	t  	nil	t)  	#\4)
    ((nil	t  	nil	t)  	#\5)
    ((t  	nil	nil	t)  	#\6)
    ((nil	nil	nil	t)  	#\7)
    ((t  	t  	t  	nil)	#\8)
    ((nil	t  	t  	nil)	#\9)
    ((t  	nil	t  	nil)	#\a)
    ((nil	nil	t  	nil)	#\b)
    ((t  	t  	nil	nil)	#\c)
    ((nil	t  	nil	nil)	#\d)
    ((t  	nil	nil	nil)	#\e)
    ((nil	nil	nil	nil)	#\f)))

(defconstant l #C(0 1))
(defconstant r #C(0 -1))

(defun maze-walk (path1 path2)
  ;; trim leading W
  (assert (char= (char path1 0) #\W))
  (setf path1 (subseq path1 1 (length path1)))
  (assert (char= (char path2 0) #\W))
  (setf path2 (subseq path2 1 (length path2)))

  (let ((p #C(0 0)) (v #C(1 0)) (m (make-hash-table :test #'equal))
        (imin 0) (imax 0) (jmin 0) (jmax 0) skip)

    (loop for d across path1
         
       when (< (realpart p) imin) do (setf imin (realpart p))
       when (> (realpart p) imax) do (setf imax (realpart p))
       when (< (imagpart p) jmin) do (setf jmin (imagpart p))
       when (> (imagpart p) jmax) do (setf jmax (imagpart p))
         
       when (char= d #\W) do
         (unless skip (setf (gethash (cons p (* l v)) m) t))
         (setf skip nil)
       (incf p v)

       when (char= d #\R) do
         (setf (gethash (cons p (* l v)) m) t)
         (setf v (* r v))
         

       when (char= d #\L) do (setf v (* l v) skip t))

    (setf v (- v))
    (incf p v)

    (loop for d across path2
         
       when (< (realpart p) imin) do (setf imin (realpart p))
       when (> (realpart p) imax) do (setf imax (realpart p))
       when (< (imagpart p) jmin) do (setf jmin (imagpart p))
       when (> (imagpart p) jmax) do (setf jmax (imagpart p))
         
       when (char= d #\W) do
       (unless skip (setf (gethash (cons p (* l v)) m) t))
         (setf skip nil)
       (incf p v)

       when (char= d #\R) do
         (setf (gethash (cons p (* l v)) m) t)
         (setf v (* r v))

       when (char= d #\L) do (setf v (* l v) skip t))

    (loop for i from imin to imax do
         (loop for j from jmin to jmax do
              (princ
               (second
                (assoc (loop for d in '(#C(-1 0) #C(1 0) #C(0 -1) #C(0 1))
                          collect (gethash (cons (complex i j) d) m))
                       +code+ :test #'equalp))))
         (terpri))))

(defun maze* ()
  (dotimes (n (read))
    (format t "Case #~D:~%" (1+ n))
    (apply #'maze-walk (split-sequence:split-sequence #\Space (read-line)))))

