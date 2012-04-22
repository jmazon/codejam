(in-package #:cl-user)

(defun fly-simulate (f rr rt cr g)
  (declare (optimize speed)
           (double-float f)
           (double-float rr)
           (double-float rt)
           (double-float cr)
           (double-float g))
  (/ (* 4
        (loop with r2 = (* rr rr)
           and -r2 = (* (- rr rt f) (- rr rt f))
           and -c = (+ cr f)
           and u = (+ g cr cr)
           and 2c = (+ cr cr f f)

           for i fixnum below 10000000
           and x double-float = (* rr (/ (random most-positive-fixnum)
                                         (coerce most-positive-fixnum 'float)))
           and y double-float = (* rr (/ (random most-positive-fixnum)
                                         (coerce most-positive-fixnum 'float)))
           for m2 double-float = (+ (* x x) (* y y))
           count (when (<= m2 r2)
                   (or (>= m2 -r2)
                       (<= (rem (+ x -c) u) 2c)
                       (<= (mod (+ y -c) u) 2c))) fixnum
           ))
     (* 10000000 pi)))

(defun read-float ()
  (coerce (read) 'double-float))

(defun fly ()
  (dotimes (i (read))
    (format t "~&Case #~D: ~F" (1+ i)
            (fly-simulate (read-float) (read-float)
                          (read-float) (read-float) (read-float)))))

(defun fly-tests ()
  (with-input-from-string (*standard-input* "5
0.25 1.0 0.1 0.01 0.5
0.25 1.0 0.1 0.01 0.9
0.00001 10000 0.00001 0.00001 1000
0.4 10000 0.00001 0.00001 700
1 100 1 1 10")
    (fly)))