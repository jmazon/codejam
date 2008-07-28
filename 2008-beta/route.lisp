(in-package #:cl-user)

(defvar *start*)
(defvar *adjacency*)
(defvar *routes*)

(defun closest (open dist)
  (loop with nmin and dmin = most-positive-fixnum
       for n in open
       for d = (gethash n dist)
       when (< d dmin) do (setf nmin n dmin d)
       finally (return nmin)))

(defun dijkstra ()
  (let ((open (list *start*))
        (dist (make-hash-table))
        (closed (make-hash-table))
        (previous (make-hash-table)))
    (setf (gethash *start* dist) 0)
    (loop for o = (closest open dist)
         while o do
         (setf open (remove o open)
               (gethash o closed) t)
         (loop for r in (remove-if #'(lambda (y) (gethash y closed))
                                   (gethash o *adjacency*)
                                   :key #'car)
              for (d . c) = r
              for n = (+ (gethash o dist) c)
              ;; ensure we process all destinations
              do (pushnew d open)
              ;; keep ties: augment previous list when shortest path found
              when (= n (gethash d dist most-positive-fixnum)) do
              (push (cons o r) (gethash d previous))
              ;; new best: replace previous list altogether
              when (< n (gethash d dist most-positive-fixnum)) do
              (setf (gethash d dist) n
                    (gethash d previous) (list (cons o r)))))
    previous))
                       
(defun paths-to (dest prev)
  (let ((paths (gethash dest prev)))
    (if paths
        (loop for (o . r) in (gethash dest prev)
           nconc (mapcar #'(lambda (p) (cons r p)) (paths-to o prev)))
        '(nil))))

(defun process-path (path-weight path road-weights)
  (loop for road in path do (incf (gethash road road-weights 0) path-weight)))

(defun process-paths (dest-weight paths road-weights)
  (loop with path-weight = (/ dest-weight (length paths))
       for path in paths
       do (process-path path-weight path road-weights)))

(defun solve ()
  (let* ((previous (dijkstra))
         (paths-to (make-hash-table))
         (dest-count
          (loop for dest being the hash-keys in previous
             do (setf (gethash dest paths-to) (paths-to dest previous))
             count t))
         (road-weights (make-hash-table)))
    (loop for paths being the hash-values in paths-to
         do (process-paths (/ dest-count) paths road-weights))
    (loop for road in *routes* collect (gethash road road-weights 0))))

(defmacro with-routes (&body body)
  `(let* ((route-count (read))
          (*start* (read))
          (*adjacency* (make-hash-table))
          (*routes*
           (loop for i below route-count
              for origin = (read)
              and destination = (read)
              and distance = (read)
              for edge = (cons destination distance)
              do (push edge (gethash origin *adjacency*))
              collect edge)))
     ,@body))

(defun route ()
  (dotimes (i (read))
    (format t "~&Case #~D:~{ ~F~}"
            (1+ i)
            (with-routes (solve)))))

(defun route* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (route))))

(defun collect-info ()
  (with-open-file (*standard-input* #P"route.in")
    (read)
    (with-routes
      (values
       *start*
       *adjacency*
       *routes*))))