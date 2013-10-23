(in-package :project-euler)

(defun show-gcd-algorithm (a b &optional (show t))
  (multiple-value-bind (quotient remainder) (floor a b)
    (if (zerop remainder)
	b
	(progn
	  (when show
	    (format t "a(~a) = b(~a).q(~a)+r(~a)~%" a b quotient remainder))
	  (show-gcd-algorithm b remainder show)))))


(defun p-108-solutions-show (n)  
  (loop for x from (1+ n) to (* 2 n) when (= 1 (numerator (- (/ 1 n) (/ 1 x))))
       collect (list (/ 1 n) (/ 1 x) (- (/ 1 n) (/ 1 x)))))

(defun p-108-solutions (n)  
  (loop for x from (1+ n) to (* 2 n) counting (= 1 (numerator (- (/ 1 n) (/ 1 x))))))
(defun p-108-solutions-fast (n)
  (let ((divs (number-divisors-square n)))
    (/ (1+ divs) 2)))

(defun duplist (l)
  (loop for e in l nconc (list e e)))
(defun number-divisors-square (n)
  (number-divisors (* n n) (duplist (cllib:divisors n))))

(defun problem-108 (&optional (requested-solutions 1000) (limit most-positive-fixnum))
  (let ((max-solutions 0)
	(n-for-max-solutions 0)
	(current-divisors 30))
    (loop for n from 48134517631200 to limit by (* 2 3 5 7 11 13) do
	 (let ((num-div 30))
	   (when (>= num-div current-divisors)
	     ;(setf current-divisors num-div)
	     (let ((solutions (p-108-solutions-fast n)))
	       (when (> solutions max-solutions)
		 (format t "found ~a solutions for ~a (~a divisors =>[~{~a~^, ~}])~%" 
			 solutions n (number-divisors n) (sort (ifactors n) '<))
		 (setf n-for-max-solutions n max-solutions solutions)
		 (when (>= solutions requested-solutions)
		   (return (list n-for-max-solutions max-solutions))))))))))