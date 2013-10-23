(in-package :project-euler)

(defun problem-57 ()
  (let ((parfrac-expansions 
	 (let ((current (+ 1 (/ 1 2))))
	   (loop for i from 1 to 999 collecting (setf current (+ 1 (/ 1 (+ 1 current))))))))
  (flet ((num-digits (x) (+ (floor (log x 10)) 1)))
    (length (remove-if-not #'(lambda (x) (> (num-digits (numerator x)) (num-digits (denominator x)))) parfrac-expansions)))))

