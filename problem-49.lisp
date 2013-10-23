(in-package :project-euler)

(defun are-digit-permutations (n1 n2 n3)
  (let* ((n1-digits (coerce (number-digits n1) 'vector))
	 (permutations (cllib:permutations-list n1-digits)))
    (and (find-if #'(lambda (x) (equalp x (coerce (number-digits n2) 'vector))) permutations)
	 (find-if #'(lambda (x) (equalp x (coerce (number-digits n3) 'vector))) permutations))))
   

(defun problem-49 ()
  (let ((primes (cllib:primes-to 20000)))
    (loop for i from 1000 to 10000 do
	  (when (cllib:primep i primes)
	    (loop for j from (1+ i) to (- 10000 i) do
		  (when (and (cllib:primep j primes)
			     (< (+ j (- j i)) 10000)
			     (cllib:primep (+ j (- j i)) primes)
			     (are-digit-permutations i j (+ j (- j i))))
		    (format t "~a~%" (list i j (+ j (- j i))))))))))
