(in-package #:cl-user)

(defvar *S* 5)
(defvar *matrix*) ; coords to index
(defvar *adj*)    ; index to neighbors
(defvar *coords*) ; index to coords

(defun middle (&optional (s *S*))
  (1- (/ (1+ s) 2)))

(defun row-size (i &optional (s *S*))
  (- s (abs (- i (middle s)))))

(defun make-hexagon (&optional (s *S*))
  (let ((matrix (make-array (list s s) :initial-element nil))
        (adj (make-hash-table))
        (coords (make-hash-table)))
    (flet ((candidates (i j)
             (list (list (1- i) (1- j))
                   (list (1- i) j)
                   (list i (1- j))
                   (list i (1+ j))
                   (list (1+ i) j)
                   (list (1+ i) (1+ j))))
           (get-valid-c (c) (destructuring-bind (i j) c
                              (and (< -1 i s) (< -1 j s)
                                   (aref matrix i j)))))
      (loop for r below s with i = 0 do
           (loop with offset = (max 0 (- r (middle s)))
              for c below (row-size r)
              do (setf (aref matrix r (+ c offset)) (incf i))))
      (loop for i below s do
           (loop for j below s
              for p = (aref matrix i j)
              when p do
              (setf (gethash p adj)
                    (delete nil (mapcar #'get-valid-c (candidates i j)))
                    (gethash p coords) (list i j)))))
    (values matrix adj coords)))

(defvar *pos* (make-array *S* :initial-contents '(1 2 5 15 19)))
(defvar *val* (make-array *S* :initial-contents '(1 3 3  0  0)))

(let ((h (make-hash-table :test #'equal)))
  (defun checker-dist (&rest args)
    (declare (optimize speed))
    (or (gethash args h)
        (setf (gethash args h)
              (destructuring-bind (p1 p2) args
                (destructuring-bind (i1 j1) p1
                  (declare (fixnum i1) (fixnum j1))
                  (destructuring-bind (i2 j2) p2
                    (declare (fixnum i2) (fixnum j2))
                    (let ((di (- i2 i1)) (dj (- j2 j1)))
                      (if (= (signum di) (signum dj))
                          (max (abs di) (abs dj))
                          (+ (abs di) (abs dj)))))))))))

(defun pos-dist (p1 p2)
  (loop for c1 across p1 and c2 across p2 and v across *val*
       sum (* v (checker-dist (gethash c1 *coords*) (gethash c2 *coords*)))))

(defvar *closed*)

(defun best-neighbor (node goal
                      &aux (best node) (best-score most-positive-fixnum))
  (dolist (s (loop for i below *S* nconc
                  (loop for j below i collect (list i j))))
    (let ((next (make-array *S* :initial-contents node)))
      (rotatef (aref next (first s)) (aref next (second s)))
      (unless (gethash next *closed*)
        (let ((next-score (pos-dist next goal)))
          (when (< next-score best-score)
            (setf best next
                  best-score next-score))))))
  (list best best-score))

(defun hill (start goal)
  (declare (optimize speed))
  (loop for x = start then next
       and score = (pos-dist start goal) then next-score
       for (next next-score)  = (best-neighbor x goal)
       while (<= next-score score)
     do (setf (gethash x *closed*) t)
       finally (return score)))

(defun make-goals ()
  (loop for i below *S*
       collect (aref *matrix* i i) into d1
       collect (aref *matrix* (middle) i) into d2
       collect (aref *matrix* i (middle)) into d3
       finally (return (list (coerce d1 'vector)
                             (coerce d2 'vector)
                             (coerce d3 'vector)))))

(defun hexagon ()
  (let* ((*pos* (with-input-from-string (s (read-line))
                  (coerce (loop for i = (read s nil) while i collect i)
                          'vector)))
         (*val* (with-input-from-string (s (read-line))
                 (coerce (loop for i = (read s nil) while i collect i)
                         'vector)))
         (*S* (length *pos*))
         (*closed* (make-hash-table :test #'equalp)))
    (multiple-value-bind (*matrix* *adj* *coords*) (make-hexagon)
      (loop for goal in (make-goals)
           minimize (hill goal *pos*)))))

(defun hexagon* ()
  (dotimes (i (read))
    (format t "~&Case #~D: ~D"
            (1+ i) (hexagon))))

(defun hexagon** (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (hexagon*))))
