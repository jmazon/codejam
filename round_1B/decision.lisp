(defun action ()
  (dotimes (i (read))
    (format t "Case #~D:~%" (1+ i))
    (read)                              ; ignore lines
    (let ((tree (read)))
      (dotimes (a (read))
        (let* ((animal (read))
               (n (read))
               (features
                (loop for i below n collect (read))))
          (format t "~,7F~%" (evaluate tree features 1)))))))

(defun evaluate (tree features p)
  (destructuring-bind (weight &optional f t1 t2) tree
      (if f
          (if (member f features)
              (evaluate t1 features (* p weight))
              (evaluate t2 features (* p weight)))
          (* p weight))))

(with-open-file (*standard-input* #p"decision-large.in")
  (action))
