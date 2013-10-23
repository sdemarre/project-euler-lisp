(in-package :project-euler)
(defun solutions-for-problem-39 (p)
  (loop for a from 1 to (/ p 2)
	for b = (/ (- (* 2 p a) (* p p))
		   (- (* 2 a) (* 2 p)))
	while (> b a)
	when (= b (truncate b))
	collect (list a b (- p a b))))
  
(defun problem-39 ()
  (let ((max-solutions 0)
	(max-p 0))
    (loop for p from 5 to 999 do 
	  (let ((num-solutions (length (solutions-for-problem-39 p))))
	    (when (> num-solutions max-solutions)
	      (setf max-solutions num-solutions)
	      (setf max-p p))))
    (list max-p max-solutions)))


















