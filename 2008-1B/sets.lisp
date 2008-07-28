(in-package #:cl-user)

(defparameter *primes*
  (loop with a = (make-array 1000000 :element-type 'boolean :initial-element t)
       initially (setf (aref a 0) nil
                       (aref a 1) nil)
       for i from 2 below 1000000
       when (aref a i)
       do (loop for j from (+ i i) below 1000 by i
               do (setf (aref a j) nil))
       finally (return (loop for k below 1000 when (aref a k) collect k))))

(defun solve (a b p)
  (let ((set (make-array (1+ (- b a))))
        (primes (loop for pr on *primes*
                   while (< (first pr) p)
                   finally (return pr))))
    (macrolet ((s (i) `(aref set (- ,i a))))
      (loop for i from a to b do (setf (s i) i))
      (loop for pr in primes
         for f = (* pr (ceiling a pr))
         when (> pr (- b a)) return nil
         do (loop with m = (min pr (loop for c from f to b by pr
                                      minimize (s c)))
               for c from f to b by pr
               do (setf (s c) m))))
    (let ((acc 0))
      (reduce #'(lambda (x y) (when (/= x y) (incf acc)) y)
              (sort set #'<)
              :initial-value 0)
      acc)))

(defun problem ()
  (declare (optimize debug))
  (dotimes (i (read))
    (format t "~&Case #~D: ~D" (1+ i)
            (solve (read) (read) (read)))))

(defun problem* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (problem))))
