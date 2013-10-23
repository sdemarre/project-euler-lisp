(in-package :project-euler)
(defun problem-187 (&optional (limit 100000000))
  (let ((v (make-array `(,limit) :element-type 'standard-char :initial-element #\b)))
    (loop for i in (cllib:primes-to (/ limit 2)) do
	 (loop for j in (cllib:primes-to (/ limit 2)) do
	      (let ((p (* i j)))
		(when (> p limit)
		  (return))
		(setf (aref v p) #\a))))
    (count-if #'(lambda (c) (char= c #\a)) v)))
				   