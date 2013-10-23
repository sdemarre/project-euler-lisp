(in-package :project-euler)
(defun problem-16 (&optional (power 1000))
  (digit-sum (expt 2 power)))
