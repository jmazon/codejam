(in-package #:cl-user)

(defconstant +mod+ 1000000007)

(defun generate (n m x y z a)
  (loop for i below n
       collect (aref a (mod i m))
       do (setf (aref a (mod i m))
                (mod (+ (* x (aref a (mod i m)))
                        (* y (1+ i)))
                     z))))

(defun input ()
  (let* ((n (read))
         (m (read))
         (x (read))
         (y (read))
         (z (read))
         (a (loop with a = (make-array m) for i below m
               do (setf (aref a i) (read))
                 finally (return a))))
    (generate n m x y z a)))


(defun solve (speeds)
  (loop with ss = (make-hash-table)
     for speed in speeds
     do (setf (gethash speed ss)
              (mod (+ (gethash speed ss 0)
                      (loop for s below speed
                         sum (gethash s ss 0))
                      1)
                   +mod+))
     finally (return (mod
                      (loop for n being the hash-values in ss sum n)
                      +mod+))))

(defun problem ()
  (declare (optimize debug))
  (dotimes (n (read))
    (format t "~&Case #~D: ~D"
            (1+ n)
            (solve (input)))))