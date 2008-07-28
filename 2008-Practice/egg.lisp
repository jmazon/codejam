(in-package #:cl-user)

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache) 
                    (apply fn args)))))))

(defun solvable-db (d b)
  (cond ((zerop d) d)
        ((zerop b) b)
        (t (+ 1
              (solvable-db (1- d) b)
              (solvable-db (1- d) (1- b))))))

(defun solvable-fb (f b)
  