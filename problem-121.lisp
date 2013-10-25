(in-package :project-euler)

(defun chance-to-win-game-121 (n)
  (/ (iter (for i from 0 to (1- (expt 2 n)))
	 (let ((bin (number-to-binary i n)))
	   (when (> (funcall #'count 1 bin) (floor n 2))
	     (summing (reduce #'* (iter (for turn from 2)
					(for success in bin)
					(if (= success 1)
					    (collect 1)
					    (collect (1- turn))))))))) 
     (fact (1+ n))))

(defun fast-chance-to-win-game-121 (n)
  (/ (iter (for i from 0 to (1- (expt 2 n)))	   
	   (let ((game i))
	     (when (> (logcount game) (floor n 2))
	       (summing (iter (for turn from 2 to (1+ n))
			      (multiply (if (zerop (mod game 2))
					    (1- turn)
					    1))
			      (setf game (floor game 2)))))))
     (fact (1+ n))))

(defun problem-121 (&optional (n 15))
  (floor (/ 1 (fast-chance-to-win-game-121 n))))
