(in-package #:cl-user)

(defun get-trees ()
  (let ((n (read))
        (a (read))
        (b (read))
        (c (read))
        (d (read))
        (x0 (read))
        (y0 (read))
        (m (read)))
    (loop for x = x0 then (mod (+ (* a x) b) m)
       and y = y0 then (mod (+ (* c y) d) m)
       and i below n
       for v = (cons x y)
       collect v)))

(defun solve (trees)
  (loop for ((x1 . y1) . trees1) on trees sum
       (loop for ((x2 . y2) . trees2) on trees1 sum
            (loop for (x3 . y3) in trees2
                 count (and (zerop (mod (+ x1 x2 x3) 3))
                            (zerop (mod (+ y1 y2 y3) 3)))))))

(defun problem ()
  (dotimes (i (read))
    (format t "~&Case #~D: ~D"
            (1+ i)
            (solve (get-trees)))))

(defun problem* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (problem))))
