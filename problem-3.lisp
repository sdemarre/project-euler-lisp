(in-package :project-euler)
(defun problem-3 (&optional (some-number 600851475143))
  (apply #' max (ifactor some-number)))
