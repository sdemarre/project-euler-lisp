(in-package :project-euler)

(defun safe-prime-p (n)
  (and (> n 0) (prime-p n)))

(defun num-primes-with-quadratic-equation (a b)
  (flet ((prime-generator (n) (+ (* n n) (* a n) b)))
    (let ((index 0))
      (while (safe-prime-p (prime-generator index))
	(incf index))
      index)))
   
(defun problem-27 ()
  (let ((max-primes 0)
	(max-a 0)
	(max-b 0))
    (loop for a from -999 to 999 do
	  (loop for b from -999 to 999 do
		(let ((num-primes (num-primes-with-quadratic-equation a b)))
		  (when (> num-primes max-primes)
		    (setf max-primes num-primes)
		    (setf max-a a)
		    (setf max-b b)
		    (format t "~a~%" (list :num max-primes :a a :b b :product (* a b)))))))
    (list :num max-primes :a max-a :b max-b :product (* max-a max-b))))
