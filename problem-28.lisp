(in-package :project-euler)
(defun problem-28 (&optional (n 1001))
  (if (= 1 n)
      1
      (+ (problem-28 (- n 2))
	 (* 4 (- n 2) (- n 2))
	 (* 10 (1- n)))))
