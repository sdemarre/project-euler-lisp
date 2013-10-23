(in-package :project-euler)
(defun problem-1 (&optional (max 1000))
  (apply #'+ (remove-if-not #'(lambda (x) (or (= (mod x 3) 0) (= (mod x 5) 0)))
			    (range 1 (1- max)))))

