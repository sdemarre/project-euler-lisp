(in-package :project-euler)

(defun has-problem-43-property-p (number)
  (flet ((check-property (factor prime) (zerop (mod (mod (floor number factor) 1000) prime))))
    (and 
	 (check-property       1 17)
	 (check-property      10 13)
	 (check-property     100 11)
	 (check-property    1000  7)
	 (check-property   10000  5)
	 (check-property  100000  3)
	 (check-property 1000000  2))))

(defun problem-43 ()
  (let ((sum 0))
    (with-permutations-swap (permutation #(0 1 2 3 4 5 6 7 8 9))
      (let ((number (vector-digits-to-number permutation)))
	(when (has-problem-43-property-p number)
	  (incf sum number))))
    sum))

