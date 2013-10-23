(in-package :project-euler)
(defun problem-72 (&optional (limit 1000000))
  (loop for i from 1 to limit sum (totient i)))