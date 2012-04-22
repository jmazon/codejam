(in-package #:cl-user)

(defun universe-read-test ()
  "Read input to two lists of symbols: engines and queries."
  (list
   (loop for i below (read) collect (intern (read-line)))
   (loop for i below (read) collect (intern (read-line)))))

(defun universe-preprocess (args)
  "Convert input lists to list of longest runs.  Runs are represented as an array, with array[run-start] = run-end."
  (if (null (second args))
      #(0)
      (loop
         with (engines queries) = args
         with run-start = (make-hash-table)
         with run-end = (make-array (length queries))

         ;; one query at a time,
         for q in queries
         for i upfrom 0
         for s = (gethash q run-start 0)

         ;; close the matching engine's run
         when (< s i) do (setf (aref run-end s) (1- i))
         do (setf (gethash q run-start) (1+ i))

         ;; the last run is taken as the biggest available
         ;; chosen from all engines.
         finally (setf (aref run-end
                             (reduce #'min
                                     (mapcar #'(lambda (e)
                                                 (gethash e run-start 0))
                                             engines)))
                       i)
         (return run-end))))
                   
(defun universe-dijkstra (run-end &aux (l (array-dimension run-end 0)))
  "Dijkstra's search of minimum path length from 0 to l-1."
  ;; There's got to be a quicker safe way of doing this...

  (let ((dist (make-array l :initial-element most-positive-fixnum))
        ;; (previous (make-hash-table))
        (open (loop for i below l collect i)))

    (labels ((closest () "Open run with the shortest distance."
               (loop with md = most-positive-fixnum and mn
                  for n in open
                  for d = (aref dist n)
                  when (< d md)
                  do (setf md d mn n)
                  finally (return mn)))
             (neighbors (n) "Runs reachable after the specified one."
               (loop for i from (1+ n) to (1+ (aref run-end n)) collect i)))

      (loop
         ;; source node: run starting at 0
         initially (setf (aref dist 0) 0)
         while open for current = 0 then (closest)

         ;; goal node: run ending at end of queries
         when (= (aref run-end current) (1- l))
         return (return (aref dist current))
         ;;  (loop for n = current then (gethash n previous)
         ;;     while n collect n)

         ;; relax current node
         do (loop
               initially (setf open (delete current open))
               for next in (neighbors current)
               for next-dist = (1+ (aref dist current))
               when (< next-dist (aref dist next))
               do (setf (aref dist next) next-dist
                        ;; (gethash next previous) current
                        ))))))

(defun universe ()
  (dotimes (n (read))
    (format t "~&Case #~D: ~D"
            (1+ n)
            (universe-dijkstra
             (universe-preprocess
              (universe-read-test))))))

(defun universe* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output
                                       :if-exists :supersede)
      (universe))))
