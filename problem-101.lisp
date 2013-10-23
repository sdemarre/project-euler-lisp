(in-package :project-euler)

(defun fit-for-bop (generating-poly bop-index)
  (let* ((datapoints (loop for i from 1 to bop-index collect (list i (eval-poly generating-poly i))))
	 (bop (interpolating-poly datapoints)))
    (loop for i = 1 then (1+ i)
       when (not (= (eval-poly generating-poly i) (eval-poly bop i)))
	 return (eval-poly bop i))))

(defun problem-101 (&optional (generating-poly '(1 -1 1 -1 1 -1 1 -1 1 -1 1)))
  (let ((max-order (1- (length generating-poly))))
    (loop for order from 1 to max-order summing (fit-for-bop generating-poly order))))
    