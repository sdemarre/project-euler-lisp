(in-package :project-euler)

(defun problem-63 ()
  (let ((count 0))
    (loop for power from 1 to 22 do
	  (loop for base from 1 to 10 do
		(when (= (length (number-to-digits (expt base power))) power)
		  (incf count)
		  (format t "~a^~a = ~a [~a digits]~%" base power (expt base power) power))))
    count))
