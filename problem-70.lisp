(in-package :project-euler)

	    
(defun problem-70 (limit)
  (let ((result 100)
	(index 0))
    (loop for i from 2 to limit do
	  (let ((tot (totient i)))
	    (when (and (< (/ i tot) result)
		       (digits-permutation-p tot i))
	      (setf result (/ i tot))
	      (setf index i))))
    (cons result index)))
