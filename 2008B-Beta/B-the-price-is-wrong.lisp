(in-package #:cl-user)

(defun price-read-line ()
  (with-input-from-string (s (read-line))
    (loop for i = (read s nil) while i collect i)))

(defun price-read ()
  (let ((items (price-read-line))
        (prices (price-read-line))
        (indices (make-hash-table)))
    (loop for i upfrom 0 and it in items
         do (setf (gethash it indices) i))
    (mapcar #'car
            (sort (mapcar #'(lambda (item price)
                              (cons (gethash item indices) price))
                          items prices)
                  #'<
                  :key #'cdr))))

(defun price-neighbors (x)
  (loop for i in x collect (remove i x)))

(defun price-astar (start)
  (declare (optimize debug))
  (let ((closed (make-hash-table :test #'equal))
        (open (list start))
        (-g- (make-hash-table :test #'equal))
        (-h- (make-hash-table :test #'equal))
        (come-from (make-hash-table :test #'equal)))
    (setf (gethash start -g-) 0)
    (labels ((f (x) (+ (g x) (h x)))
             (g (x) (gethash x -g- most-positive-fixnum))
             (h (x) (or (gethash x -h-)
                        (setf (gethash x -h-)
                              (loop for i upfrom 0 and p in x count (/= i p)))))
             (lowest-f () (loop
                             with fmin = most-positive-fixnum and xmin
                             for x in open
                             for f = (f x)
                             when (< f fmin) do (setf fmin f xmin x)
                             finally (return xmin)))
             (goalp (x) (loop for (a b) on x while b always (< a b)))
             (trace-back (x)
               (nreverse (loop for y = x then z for z = (gethash y come-from)
                            while z nconc (set-difference z y))))
             (closed (x) (gethash x closed)))
      (loop while open
         for x = (lowest-f)
         do (format t "~&~A" x)
         when (goalp x) return (trace-back x)
         do
         (setf open (delete x open :test #'equal)
               (gethash x closed) t)
         (loop for y in (delete-if #'closed (price-neighbors x))
            for g = (1+ (g x))
            do          (format t "~&  ~A" y)
            unless (member y open :test #'equal)
            do (setf open (nconc open (list y)))
            when (< g (g y))
            do (setf (gethash y come-from) x
                     (gethash y -g-) g))))))
