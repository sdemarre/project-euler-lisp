(in-package :project-euler)
(defun problem-10 (n)
  (let ((primetab (make-prime-list-for-range n)))
   (iter (for i from 1 to n) 
	 (when (aref primetab i)
	   (summing i)))))

