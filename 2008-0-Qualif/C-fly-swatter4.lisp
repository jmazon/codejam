(in-package #:cl-user)

(defconstant +resolution+ 10000000)
(defun rand-theta ()
  (/ (* 2 pi (random +resolution+))
     +resolution+))

(defun rand-r (rmax)
  (let ((s (coerce (/ (random +resolution+) +resolution+) 'float)))
    (* rmax s s)))