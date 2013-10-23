(in-package :project-euler)

(defun problem-115 ()
  (let ((solution))
    (loop for i = 51 then (1+ i) until solution do
	 (when (> (problem-114 i 50) (expt 10 6))
	   (setf solution i)))
    solution))