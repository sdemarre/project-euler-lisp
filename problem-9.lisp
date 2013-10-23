(in-package :project-euler)
(defun pythagorean-triplet (a b c)
  (= (square c) (+ (square a) (square b))))

(defun problem-9 (&optional (sum 1000))
    (loop for a from 1 to (floor sum 2) do
	  (loop for b from (+ a 1) to (- sum a) do
		(loop for c from (+ b 1) to (- sum (+ a b)) do
		      (when (and (= sum (+ a b c)) (pythagorean-triplet a b c))
			(format t "FOUND!! ~a,~a,~a, product = ~a~%" a b c (* a b c)))))))
