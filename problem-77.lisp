(in-package :project-euler)


(defun primes-vector (up-to)
  (let ((result (make-array (list (+ 2 up-to)) :initial-element nil))
	(primes (cllib:primes-to up-to)))
    (loop for i from 2 to up-to when (cllib:primep i primes) do
	 (setf (aref result i) t))
    result))

(defun is-prime (number primes)
  (aref primes number))

(defun next-prime (prime primes)
  (loop for result = (1+ prime) then (1+ result) when (is-prime result primes) return result))

(defun number-of-ways-to-make-sum-with-primes (number minimum primes)
  (cond  ((zerop number) 1)
	 ((= 1 number) 0)
	 ((< number 0) 0)
	 (t   (loop for first-element = minimum then (next-prime first-element primes) until (> first-element number) summing
		   (number-of-ways-to-make-sum-with-primes (- number first-element) first-element primes)))))

(defun problem-77 (&optional (possible-sums 5000))
  (let ((primes (primes-vector possible-sums)))
    (loop for i = 3 then (1+ i) until (>= i possible-sums) when (> (number-of-ways-to-make-sum-with-primes i 2 primes) possible-sums)
	 return i)))




