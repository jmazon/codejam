(in-package #:cl-user)

(defun minimize (l)
  (setf l (sort l #'< :key #'car))
  (loop with slope = (reduce #'+ (mapcar #'/ (mapcar #'cdr l)))
       for (x . rest) on l
       for newslope = (- slope (/ 2 (cdr x)))
       do (format t "~&~A ~A ~A" x slope newslope)
       when (minusp newslope) return x
       when (zerop newslope) return (/ (+ (car x) (caar rest)) 2)
       do (setf slope newslope)))