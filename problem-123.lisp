(in-package :project-euler)

(defun problem-123 (&optional (limit (expt 10 10)))
  (let ((primes (cllib:primes-to (isqrt limit))))
    (loop for index = 1 then (1+ index) 
	 for prime in primes 
	 when (and (> index 7037) (> (mod (+ (* index prime (if (oddp index) 1 -1))  (* index prime) 1 (if (evenp index) 1 -1)) (expt prime 2)) limit))
	 return index)))
	 