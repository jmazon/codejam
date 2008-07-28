(in-package #:cl-user)

;;; Special cases to take care of:
;;;  f > g/2

(defparameter *f* 0.25)
(defparameter *rr* 1.0)
(defparameter *t* 0.1)
(defparameter *cr* 0.01)
(defparameter *g* 0.9)

(defvar *u*)
(defun fly-adjust-parameters ()
  (decf *rr* *t*) ; racket inside radius
  (setf *u* (+ *g* *r* *r*)) ; unit square side
  ;; substract fly radius from all areas
  (decf *rr* *f*)
  (decf *g* *f*)
  (decf *g* *g*)
  (incf *cr* *f*)
  (incf *cr* *f*))
  
(defun fly-racket-intersection (x)
  (sqrt (- (* *rr* *rr*) (* x x))))

(defun fly-square ()
  (* *g* *g*))

(defun fly-strap (y1 y2 &aux res)
  (let* ((ya (+ y1 *cr*))
         (yb (- y2 *cr*))
         (x1 (fly-racket-intersection y1))
         (x2 (fly-racket-intersection y2))
         x)

    (multiple-value-bind (full-squares partial-square)
        (truncate (- x2 y2) *u*)

      (setf x (- x2 partial-square))
      
      (push (* (+ full-squares .5) (fly-square)) res)


      