(defun diff (args)
  (loop for (a b . r) on (sort args #'<) when b collect (- b a)))

(defun period (steps)
  (reduce #'gcd steps))

(defun anniversary (base period &aux (r (rem base period)))
  (if (zerop r) 0 (- period r)))

(defun fair (dates)
  (anniversary (first dates)
               (period (diff dates))))

(defun test-case ()
  (fair (loop for i below (read) collect (read))))

(defun test ()
  (dotimes (i (read))
    (format t "Case #~D: ~D~%"
            (1+ i)
            (test-case))))

(defun wrapper (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (test))))
