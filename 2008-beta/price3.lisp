(in-package #:cl-user)

(defun tokenize ()
  (with-input-from-string (s (read-line))
    (loop for i = (read s nil) while i collect i)))

(defvar *items* ' (3 0 2 4 1))
(defvar *reverse* #(CODE JAM FOO BAR GOOGLE))
(defvar *start* ' (1 0 3 2 4))
(defvar *length* 5)

(defmacro with-prices (&body body)
  `(let* ((*items* (tokenize))
          (*reverse* (make-array (length *items*) :initial-contents *items*))
          (prices (tokenize))
          (*start* (mapcar #'(lambda (x) (position (cdr x) *items*))
                         (sort (mapcar #'cons prices *items*) #'< :key #'car)))
          (*length* (length *items*)))
     (setf *items* (mapcar #'(lambda (x) (position x *items*))
                         (sort (copy-list *items*) #'string<
                               :key #'symbol-name)))
     ,@body))

(defun longest-subseq (l &optional (sl 1) (slm 0))
  (declare (optimize (speed 3) (safety 0))
           (fixnum sl) (fixnum slm))
  (let ((a (first l)) (b (second l)))
    (cond ((null l) slm)
          ((or (null b) (> (the fixnum a) (the fixnum b)))
           (longest-subseq (rest l) 1 (max sl slm)))
          (t (longest-subseq (rest l) (1+ sl) slm)))))

(defun heuristic (n) (- (length n) (longest-subseq n)))

(defun neighbors (n)
  (loop for i in *items*
       when (member i n)
       collect (cons i (remove i n))))

(defun a-star ()
  (let ((open (list *start*))
        (closed (make-hash-table :test #'equal))
        (come-from (make-hash-table :test #'equal))
        (trace (make-hash-table :test #'equal))
        (f-score (make-hash-table :test #'equal))
        (g-score (make-hash-table :test #'equal))
        (h-score (make-hash-table :test #'equal)))
    (flet ((lowest-f ()
             (loop with xmin and fmin = most-positive-fixnum
                for x in open
                for f = (gethash x f-score)
                when (< f fmin) do (setf xmin x fmin f)
                finally (return xmin)))
           (goalp (n) (zerop (heuristic n)))
           (traceback (x)
             (loop for n = x then (gethash n come-from)
                for i = (gethash n trace)
                while i collect i)))
      (setf (gethash *start* g-score) 0
            (gethash *start* f-score) (heuristic *start*))
      (loop for x = (lowest-f) while x
           when (goalp x) return (traceback x)
           do (setf open (delete x open)
                    (gethash x closed) t)
           (loop with neighbors = (delete-if #'(lambda (n) (gethash n closed))
                                             (neighbors x) :key #'cdr)
              for (e . y) in neighbors and g = (1+ (gethash x g-score))

              unless (member y open :test #'equal)
              do (setf open (nconc open (list y))
                       (gethash y h-score) (heuristic y))
                
              when (< g (gethash y g-score most-positive-fixnum))
              do (setf (gethash y come-from) x
                       (gethash y trace) e
                       (gethash y g-score) g
                       (gethash y f-score) (+ g (gethash y h-score))))))))

(defun solve ()
  (sort (mapcar #'(lambda (i) (aref *reverse* i)) (a-star))
        #'string< :key #'symbol-name))

(defun price ()
  (dotimes (i (read))
    (format t "~&Case #~D:~{~( ~A~)~}"
            (1+ i) (with-prices (solve)))))

(defun price* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (price))))
