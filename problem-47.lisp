(in-package :project-euler)


(defun problem-47 (&optional (num-primes 4) (num-consecutive-integers 4))
  (let ((current-consecutive-count 0))
    (do ((i 2 (1+ i)))
	((= num-consecutive-integers current-consecutive-count) (- i num-consecutive-integers))
      (let ((divisors (fast-divisors i)))
	(if (and (= (length divisors) num-primes)
		 (apply #'/= divisors))
	    (incf current-consecutive-count)
	    (setf current-consecutive-count 0))))))
