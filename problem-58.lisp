(in-package :project-euler)

(defun problem-58 (percentage)
  (let ((diagonal-elements 5)
	(number-primes 3)
	(side-length 3)
	(primes (cllib:primes-to 100000)))
    (while (> (/ number-primes diagonal-elements) percentage)
      (incf side-length 2)
      (incf diagonal-elements 4)
      (let* ((bottom-right (* side-length side-length))
	     (1-side-length (1- side-length))
	     (bottom-left (- bottom-right 1-side-length))
	     (top-left (- bottom-left 1-side-length))
	     (top-right (- top-left 1-side-length)))
	(when (cllib:primep top-right primes)
	  (incf number-primes))
	(when (cllib:primep top-left primes)
	  (incf number-primes))
	(when (cllib:primep bottom-left primes)
	  (incf number-primes))))
    side-length))
