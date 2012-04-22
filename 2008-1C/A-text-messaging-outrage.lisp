(in-package #:cl-user)

(defun solve (k freq)
  (loop for i upfrom k and key in (sort freq #'>)
       sum (* (truncate i k) key)))

(defun input ()
  (let* ((p (read))
         (k (read))
         (l (read))
         (freq (loop for i below l collect (read))))
    (assert (>= (* p k) l))
    (list k freq)))

(defun problem ()
  (dotimes (n (read))
    (format t "~&Case #~D: ~D"
            (1+ n)
            (apply #'solve (input)))))

(defun problem* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (problem))))
