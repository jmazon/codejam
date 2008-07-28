(in-package #:cl-user)

(defun brute (n)
  (mod (truncate (expt (+ 3 (sqrt 5d0)) n)) 1000))

(defun coeffs (n)
  (loop for a = 1 then (+ (* 3 a) (* 5 b))
       and b = 0 then (+ a (* 3 b))
       and i below n
       when (>= a md) do (setf a (mod a md))
       when (>= b md) do (setf b (mod b md))
       finally (return (values (mod a md) (mod b md)))))

(defparameter id #2A((1 0) (0 1)))
(defparameter mat #2A((3 5) (1 3)))

(defun mult (m1 m2)
  (assert (= (array-dimension m1 1) (array-dimension m2 0)))
  (loop with m = (make-array (list (array-dimension m1 0)
                                   (array-dimension m2 1)))
       for i below (array-dimension m1 0) do
       (loop for j below (array-dimension m2 1) do
            (setf (aref m i j)
                   (loop for k below (array-dimension m1 1)
                      sum (* (aref m1 i k) (aref m2 k j)))))
     finally (return m)))

(defun power (n)
  (cond ((zerop n) id)
        ((oddp n) (mult mat (power (1- n))))
        (t (let ((m (power (/ n 2))))
             (mult m m)))))

(defun solve (n)
  (let ((res (mult (power n) #2A((1) (0)))))
    (mod (truncate (+ (aref res 0 0) (* (aref res 1 0) (sqrt 5)))) 1000)))


(defun wrapper ()
  (dotimes (i (read))
    (format t "~&Case #~D: ~3,'0D"
            (1+ i)
            (solve (read)))))

(defun wrapper* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (wrapper))))