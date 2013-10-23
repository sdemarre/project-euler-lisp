(in-package :project-euler)
(defun problem-56 ()
  (loop for a from 1 to 100 maximize 
	(loop for b from 1 to 100 maximize (reduce #'+ (number-to-digits (expt a b))))))
