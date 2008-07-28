;; Résultat des courses: bfs marche sur small.
;; biggest-seq n'est que le début de qqch qui pourrait marcher sur large.

(in-package #:cl-user)

(declaim (optimize speed))

(defun tokenize ()
  (with-input-from-string (s (read-line))
    (loop for i = (read s nil) while i collect i)))

(defvar *state*)
(defvar *items*)

(defmacro with-problem-input (&body body)
  `(let ((items (tokenize))
         (prices (tokenize)))
     (let ((*state* (mapcar #'cons items prices))
           (*items* (sort items #'string< :key #'symbol-name)))
       ,@body)))

(defun biggest-sequence (&optional (state *state*) &key (key #'cdr))
  (loop
     with seq-b = 0 and seq-e and msb and mse = 0
     for i upfrom 0 and (a b) on state
       do (print (cons a b))
     unless (> (if b (funcall key b) 0)
               (funcall key a))
     do (setf seq-e i)
     when (> seq-e mse) do (setf msb seq-b mse seq-e seq-b (1+ i) seq-e 0)
     finally (return (values msb mse))))

(defun goalp (n)
  (loop for (a b) on (mapcar #'cdr n)
     while b always (< a b)))

(defun neighbors (n)
  (loop for i in *items*
       when (member i n :key #'car)
       collect (cons (remove i n :key #'car) i)))

(defun bfs (start goalp neighbors &key (test #'equal))
  (let ((q (list start))
        (visited (make-hash-table :test test))
        (trace (make-hash-table :test test)))
    (flet ((visitedp (n) (gethash n visited))
           (visit (n p i) (setf (gethash n visited) p
                                (gethash n trace) i))
           (traceback (n)
             (nreverse
              (loop for x = n then (gethash x visited)
                 while x
                 when (gethash x trace) collect it))))
      (loop while q do
           (loop with x = (pop q)
              with nn = (delete-if #'visitedp (funcall neighbors x))
              for (y . i) in nn
              do (visit y x i)
              when (funcall goalp y) do (return-from bfs (traceback y))
              finally (setf q (nconc q (mapcar #'first nn))))))))

(defun wrapper ()
  (dotimes (i (read))
    (format t "~&Case #~D:~{ ~(~A~)~}"
            (1+ i)
            (sort (with-problem-input (bfs *state* #'goalp #'neighbors))
                  #'string< :key #'symbol-name))))

(defun wrapper* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output
                                       :if-exists :supersede)
      (wrapper))))

