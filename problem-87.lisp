(in-package :project-euler)
(defun problem-87 (&optional (limit 50000000))
  (flet ((remove-above-fun (function)
	   (remove-if #'(lambda (number) (> (funcall function number) limit)) (cllib:primes-to (isqrt limit)))))
    (let ((primes-2 (remove-above-fun #'square))
	  (primes-3 (remove-above-fun #'cube))
	  (primes-4 (remove-above-fun #'expt4))
	  (hash (make-hash-table)))
      (loop for a in primes-2 do
	   (loop for b in primes-3 do
		(loop for c in primes-4 do
		     (let ((sum (+ (square a) (cube b) (expt4 c))))
		       (when (< sum limit)
			 (setf (gethash sum hash) 1))))))
      (loop for key being the hash-keys of hash summing 1))))
	  