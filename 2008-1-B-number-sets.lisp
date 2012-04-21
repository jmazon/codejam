(in-package #:cl-user)

(defparameter *primes*
  (loop with a = (make-array 1000 :element-type 'boolean :initial-element t)
       initially (setf (aref a 0) nil
                       (aref a 1) nil)
       for i from 2 below 1000
       when (aref a i)
       do (loop for j from (+ i i) below 1000 by i
               do (setf (aref a j) nil))
       finally (return (loop for k below 1000 when (aref a k) collect k))))

(defun solve (a b p)
  (declare (optimize debug))
  (incf b)
  (let ((set (make-array (- b a)))
        (primes (loop for pr on *primes*
                     while (< (first pr) p)
                     finally (return pr))))
    (loop for i from a below b do (setf (aref set (- i a)) i))
    (loop for pr in primes
       for f = (* pr (ceiling a pr))
       when (> pr (- b a)) return nil
       when (>= f b) return nil
       do
       (loop with m = (min pr
                           (loop
                              for c from f below b by pr
                              minimize (aref set (- c a))))
            for c from f below b by pr
            do (setf (aref set (- c a)) m)))
    (length (remove-duplicates set))))

(defun problem ()
  (declare (optimize debug))
  (dotimes (i (read))
    (format t "~&Case #~D: ~D" (1+ i)
            (solve (read) (read) (read)))))

(defun problem* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (problem))))
