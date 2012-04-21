(in-package #:cl-user)

(defvar *turnaround*)

(defun train-make-station ()
  "A station is a minute-indexed array of train requirements."
  (make-array (* 24 60) :initial-element 0))

(defun train-process-trip (trip origin destination)
  "Adjust the stations to take into account a trip."
  (destructuring-bind (departure-time arrival-time) trip
    (incf (aref origin departure-time))  ; departure: one more needed train
    (let ((available-time (+ arrival-time *turnaround*)))
      (when (< available-time (* 24 60)) ; arrival: soon one less needed train
        (decf (aref destination available-time))))))             

(defun train-parse-time (time-string)
  "Convert a colon-delimited time string to a minute of day."
  (destructuring-bind (hour minute)
      (split-sequence:split-sequence #\: time-string)
    (+ (read-from-string minute)
       (* (read-from-string hour) 60))))

(defun train-parse-line ()
  "Read a train trip: two times on a line."
  (mapcar #'train-parse-time
          (split-sequence:split-sequence #\Space (read-line))))

(defun train-count-needed (station)
  "Convert a station's daily train needs, into a number of trains."
  (loop
     for i across station
     summing i into need
     maximizing need))

(defun train-test ()
  "Read the relevant input and return the test case's results."
  (let ((a-count (read))
        (b-count (read))
        (a (train-make-station))
        (b (train-make-station)))
    (dotimes (i a-count)
      (train-process-trip (train-parse-line)
                          a b))
    (dotimes (i b-count)
      (train-process-trip (train-parse-line)
                          b a))
    (list
     (train-count-needed a)
     (train-count-needed b))))

(defun train ()
  "Run a whole attempt."
  (dotimes (i (read))
    (let ((*turnaround* (read)))
      (format t "~&Case #~D: ~{~D ~D~}"
              (1+ i)
              (train-test)))))

(defun train* (in out)
  (with-open-file (*standard-input* in)
    (with-open-file (*standard-output* out :direction :output
                                           :if-exists :supersede)
      (train))))
