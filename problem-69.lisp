(in-package :project-euler)
(defun digits-permutation-p (n1 n2)
  (string= (sort (format nil "~a" n1) #'char<) 
	   (sort (format nil "~a" n2) #'char<)))

(defun totient (n)
  (apply #'* n (mapcar #'(lambda (x) (- 1 (/ 1 x))) (remove-duplicates (cllib:divisors n)))))

(defun problem-69 (&optional (limit 1000000))
  (let ((result 1)
	(index 0))
    (loop for i from 2 to limit do
	  (let ((tot (totient i)))
	    (when (> (/ i tot) result)
	      (setf result (/ i tot))
	      (setf index i))))
    (cons result index)))

