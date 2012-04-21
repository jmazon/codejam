(defun test (i)
  (let ((ot 0) (op 1)
	(bt 0) (bp 1)
	(s 0))
    (dotimes (n (read))
      (let ((r (read)) (p (read)))
	(if (eq r 'o)
	    (setf s (1+ (max s (+ ot (abs (- p op)))))
		  ot s
		  op p)
	    (setf s (1+ (max s (+ bt (abs (- p bp)))))
		  bt s
		  bp p))))
    (format t "Case #~D: ~D~%" i s)))

(dotimes (i (read))
  (test (1+ i)))
