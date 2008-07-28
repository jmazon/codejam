(in-package #:cl-user)

(declaim (optimize (speed 2)))

(defun alien-decode (nb code)
  (declare (string nb) (string code))
  (loop with decoded = 0
     for c across nb
       do (setf decoded (+ (position c code)
                           (* decoded (length code))))
       finally (return decoded)))
       
(defun alien-encode (nb code &aux res)
  (loop for (q r) = (multiple-value-list (floor nb (length code)))
     while (or (plusp q) (plusp r))
     do (push (elt code r) res)
       (setf nb q))
  (coerce res 'string))

(defun alien-convert (nb source dest)
  (alien-encode (alien-decode nb source) dest))

(defun alien* ()
  (dotimes (n (read))
    (format t "~&Case #~D: ~A"
            (1+ n)
            (apply #'alien-convert
                   (split-sequence:split-sequence #\Space
                                                  (read-line))))))
(defun alien-large ()
  (with-open-file (*standard-input* #P"A-large.in")
    (with-open-file (*standard-output* #P"A-large.out" :direction :output
                                      :if-exists :overwrite)
      (alien*))))
