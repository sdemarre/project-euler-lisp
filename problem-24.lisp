(in-package :project-euler)
(defun problem-24 (&optional (permutation-number 1000000))
  (get-nth-permutation (1- permutation-number) (range 0 9)))