;;; Superseded by version in Amadeus


(in-package #:cl-user)

(defun read-routes ()
  (let ((n (read))
        (start (read))
        (routes (make-hash-table)))
    (dotimes (i n)
      (let ((origin (read))
            (destination (read))
            (distance (read)))
        (push (cons destination distance) (gethash origin routes))))
    (values start routes)))

(defvar *start*)
(defvar *routes*)

(defmacro with-routes (&body body)
  `(multiple-value-bind (*start* *routes*) (read-routes)
     ,@body))

(defun dijkstra ()
  (declare (optimize debug))
  (let ((open (list *start*))
        (dist (make-hash-table))
        (previous (make-hash-table))
        (closed (make-hash-table)))
    (setf (gethash *start* dist) 0)
    (loop while open do
         (let ((city (loop with min and dmin = most-positive-fixnum
                        for n in open
                        for d = (gethash n dist)
                        when (< d dmin)
                        do (setf min n dmin d)
                        finally (return min))))
           (setf open (delete city open)
                 (gethash city closed) t)
           (loop for road in (remove-if #'(lambda (r) (gethash (car r) closed))
                                        (gethash city *routes*))
                for (c . d) = road
                for alt = (+ (gethash city dist) d)
                do (pushnew c open)
                when (< alt (gethash c dist most-positive-fixnum))
                do (setf (gethash c dist) alt
                         (gethash c previous) (list (cons city road)))
                when (= alt (gethash c dist most-positive-fixnum))
                do (push (cons city road) (gethash c previous)))))
    previous))

(defun paths-to (d p)
  (if (eq d *start*)
      '(nil)
      (loop for (city road) in (gethash d p)
         nconc (mapcar #'(lambda (r) (cons road r))
                       (paths-to city p)))))

(defun process-destination (dest w p)
  

(defun problem ()
  (with-routes
    (let ((paths (dijkstra))
          (weights (make-hash-table)))
      
      (loop for destination being the hash-keys in paths
           do (process-destination destination weights paths)
           count t))))
