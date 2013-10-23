(in-package :project-euler)
(defun continued-fraction-sum (continued-fraction-list)
  (let* ((reversed-list (reverse continued-fraction-list))
	 (running-value (car reversed-list)))
    (loop for value in (cdr reversed-list) do
	  (setf running-value (+ value (/ 1 running-value))))
    running-value))


(defun e-continued-fraction-value (index)
  (cond ((= 0 index) 2)
	((= 1 index) 1)
	((= 2 index) 2)
	((= 0 (mod (- index 5) 3)) (/ (* 2 (1+ index)) 3))
	(t 1)))
(defun make-e-continued-fraction-list (depth)
  (loop for index from 0 to (1- depth) collect (e-continued-fraction-value index)))
  

(defun problem-65 (&optional (depth 100))
  (let* ((fraction-list (make-e-continued-fraction-list depth))
	 (fraction (continued-fraction-sum fraction-list))
	 (numer (numerator fraction)))
    (digit-sum numer)))
