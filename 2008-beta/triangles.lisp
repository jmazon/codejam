(in-package #:cl-user)

(defun triangle-length2 (x1 y1 x2 y2)
  (let ((dx (- x2 x1)) (dy (- y2 y1)))
    (+ (* dx dx) (* dy dy))))

(defun triangle-sides2 (x1 y1 x2 y2 x3 y3)
  (sort (list (triangle-length2 x1 y1 x2 y2)
              (triangle-length2 x1 y1 x3 y3)
              (triangle-length2 x2 y2 x3 y3))
        #'<))

(defun triangle-degenerate-p (x1 y1 x2 y2 x3 y3)
  (let ((xa (- x2 x1)) (ya (- y2 y1))
        (xb (- x3 x1)) (yb (- y3 y1)))
    (zerop (- (* xa yb) (* xb ya)))))

(defun triangle-classify-sides (a2 b2 c2)
  (if (or (= a2 b2) (= b2 c2))
      'isosceles
      'scalene))

(defun triangle-classify-angles (a2 b2 c2)
  (let ((delta (- c2 a2 b2)))
    (cond ((plusp delta) 'obtuse)
          ((minusp delta) 'acute)
          (t 'right))))

(defun triangle-classify (x1 y1 x2 y2 x3 y3)
  (if (triangle-degenerate-p x1 y1 x2 y2 x3 y3)
      '(not a)
      (destructuring-bind (a2 b2 c2)
          (triangle-sides2 x1 y1 x2 y2 x3 y3)
        (list
         (triangle-classify-sides a2 b2 c2)
         (triangle-classify-angles a2 b2 c2)))))

(defun triangle-tests ()
  (print (triangle-classify 0 0 0 4 1 2))
  (print (triangle-classify 1 1 1 4 3 2))
  (print (triangle-classify 2 2 2 4 4 3))
  (print (triangle-classify 3 3 3 4 5 3))
  (print (triangle-classify 4 4 4 5 5 6))
  (print (triangle-classify 5 5 5 6 6 5))
  (print (triangle-classify 6 6 6 7 6 8))
  (print (triangle-classify 7 7 7 7 7 7)))

(defun triangle ()
  (dotimes (i (read))
    (format t "~&Case #~D: ~(~{~A ~A~}~) triangle"
            (1+ i)
            (triangle-classify (read) (read) (read) (read) (read) (read)))))

(defun triangle* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (triangle))))
