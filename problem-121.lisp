(in-package :project-euler)

(defun problem-121 (&optional (n 15))
  (floor (/ 1 (iter (for i from 0 to (expt 2 n))
		    (let ((bin (number-to-binary i n)))
		      (when (> (funcall #'count 1 bin) (floor n 2))
			(summing (reduce #'* (iter (for turn from 2)
						   (for success in bin)
						   (if (= success 1)
						       (collect (/ 1 turn))
						       (collect (- 1 (/ 1 turn)))))))))))))
