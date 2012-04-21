(in-package #:cl-user)

(defstruct leaf value)

(defstruct (node (:include leaf))
  left right
  type changeable)

(defun read-tree ()
  (let* ((m (read))
         (v (read))
         (a (make-array m :initial-element nil)))
    ; raed nodes
    (dotimes (i m)
      (setf (aref a i)
            (if (< i (floor (1- m) 2))
                (make-node :type (if (plusp (read)) 'and 'or)
                           :changeable (plusp (read)))
                (make-leaf :value (plusp (read))))))
    ; construct tree
    (dotimes (i (floor (1- m) 2))
      (setf (node-left (aref a i)) (aref a (1- (* 2 (1+ i))))
            (node-right (aref a i)) (aref a (* 2 (1+ i)))))
    (values (aref a 0) (plusp v))))

(defun evaluate-tree (tree)
  (if (node-p tree)
      (let ((left (evaluate-tree (node-left tree)))
            (right (evaluate-tree (node-right tree))))
        (setf (node-value tree)
              (if (eq (node-type tree) 'and)
                  (and left right)
                  (or left right))))
      (leaf-value tree)))

(define-condition impossible (error) ())

(defun count-changes (tree v &optional (failp t))
  (declare (optimize debug))
  (when (eq (leaf-value tree) v)
    (return-from count-changes 0))
  (unless (node-p tree)
    (if failp
        (error 'impossible)
        (return-from count-changes 10000)))
  
  (if (node-changeable tree)
      (1+ (let ((res (min (count-changes (node-left tree) v nil)
                          (count-changes (node-right tree) v nil))))
            (if (and (>= res 10000) failp)
                (error 'impossible)
                res)))
      (+  (count-changes (node-left tree) v failp)
          (count-changes (node-right tree) v failp))))

(defun solve ()
  (multiple-value-bind (tree value) (read-tree)
    (evaluate-tree tree)
    (handler-case (count-changes tree value)
      (impossible () 'impossible))))

(defun problem ()
  (dotimes (i (read))
    (format t "Case #~D: ~A~%" (1+ i) (solve))))

(defun problem* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output)
      (problem))))
