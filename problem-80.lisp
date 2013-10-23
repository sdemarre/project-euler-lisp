(in-package :project-euler)
(defun problem-80 (max number-digits)
  (loop for number from 2 to max summing
	(if (zerop (- (expt (isqrt number) 2) number))
	    0
	    (loop for digit in (subseq (number-to-digits (isqrt (* number (expt number-digits (+ 2 number-digits))))) 0 100)
		  summing digit))))
