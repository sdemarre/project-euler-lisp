(in-package :project-euler)
(defun problem-53 ()
  (let ((total 0))
    (loop for n from 23  to 100 do
	  (loop for r from 2 to (1- n) do
		(when (> (num-combinations n r)  1000000)
		  (incf total))))
    total))
