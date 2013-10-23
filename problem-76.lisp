(in-package :project-euler)

  

(defun print-partition-table (table)
  (loop for i from 0 to (1- (array-dimension table 0)) do
	(loop for j from 0 to (1- (array-dimension table 1)) do
	      (format t "~4a" (let ((it (aref table i j)))
				(if it it "."))))
	(format t "~%")))
  
(defun possible-ways-to-sum-with-table (n max table)
  (let ((tmp (aref table n max)))
    (cond (tmp tmp)
	  ((zerop n) (setf (aref table n max) 1))
	  ((= max 1) (setf (aref table n max) 1))
	  (t (setf (aref table n max)
		   (loop for i from (min max n) downto 1 summing (possible-ways-to-sum-with-table (- n i) i table)))))))
	
(defun possible-ways-to-sum (n)
  (let ((table (make-array (list (1+ n) (1+ n)) :initial-element nil)))
    (possible-ways-to-sum-with-table n n table)))

(defun possible-ways-to-sum-print-table (n)
  (let ((table (make-array (list (1+ n) (1+ n)) :initial-element nil)))
    (prog1 
	(possible-ways-to-sum-with-table n n table)
      (print-partition-table table))))

(defun problem-76 ()
  (possible-ways-to-sum 100))

