(in-package #:cl-user)
(declaim (optimize debug))

(defun fly-intersect (x r)
  (sqrt (- (* r r) (* x x))))

(defun fly-squares (x1 x2 r c g u &aux res)
  (declare (ignore x1))
  (let ((y2 (fly-intersect x2 r)))
    (multiple-value-bind (q rem) (floor (- y2 x2) u)
      (declare (ignore rem))
      (push (* g g (1+ (* 2 q))) res) ; whole squares

      (let ((ya (fly-intersect (+ x1 c))))
        
      ;; no more part square?
      (when (<= ya (- (+ y2 c) rem))
        (return-from fly-squares res))

      ;; only a single corner
      (when (<= (fly-intersect (- x2 c))
                (- (+ y2 c) rem))
        (push (fly-corner1 

(defun fly-hole (r c g)
  "First approximation: whole squares only."
  (when (or (minusp r) (minusp g))
    ;; degenerate cases
    (return-from fly-hole 0))
  
  (let ((u (+ c c g)))
    (multiple-value-bind (q rem) (floor (* r (sqrt 2)) (* u 2))
      (declare (ignore rem))
      (let ((figures (loop for i below q collect
                          (fly-squares (* i u) (* (1+ i) u) r c g u))))
        ;; try something numerically stable
        (reduce #'+ (sort figures #'<))))))

(defun fly-hit (f r w c g)
  (- 1 (/ (* 4 (fly-hole (- r w f) (+ c f) (- g f f)))
          (* pi r r))))

(defun fly ()
  (dotimes (i (read))
    (format t "~&Case #~D: ~F" (1+ i)
            (fly-hit (read) (read) (read) (read) (read)))))

(defun fly-tests ()
  (with-input-from-string (*standard-input* "5
0.25 1.0 0.1 0.01 0.5
0.25 1.0 0.1 0.01 0.9
0.00001 10000 0.00001 0.00001 1000
0.4 10000 0.00001 0.00001 700
1 100 1 1 10")
    (fly)))