(in-package :project-euler)
(defun problem-7 (&optional (x 10001))
  (let ((tmp 2))
    (dotimes (i (1- x)) (setf tmp (find-next-prime tmp)))
    tmp))

