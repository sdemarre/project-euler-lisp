(in-package :project-euler)
(defun make-format-list (title &rest strings)
  (apply #'concatenate 'string "--" title "~%" (mapcar #'(lambda (string) (concatenate 'string string " = ~a~%")) strings)))

(defun number-divisors (number &optional (prime-factors-given nil))
  (let ((prime-factors (or prime-factors-given (cllib:divisors number))))
    (let ((current-factor (first prime-factors))
	  (current-number-divisors 1)
	  (current-number-factors 1))
      (loop for factor in (rest prime-factors) do
	   (if (= factor current-factor)
	       (incf current-number-factors)
	       (progn
		 (setf current-number-divisors (* current-number-divisors (1+ current-number-factors)))
		 (setf current-factor factor)
		 (setf current-number-factors 1))))
      (* current-number-divisors (1+ current-number-factors)))))
	   
    
(defun problem-179-slow (&optional (limit 10000000))
  (let ((previous-number-divisors (number-divisors 2)))
    (loop for i from 3 to (1- limit) count (let ((num-div (number-divisors i)))
					     (prog1 
						 (= num-div previous-number-divisors)
					       (setf previous-number-divisors num-div))))))
					     
(defun problem-179 (&optional (limit (expt 10 7)))
  (let ((v (make-array (list limit) :initial-element 1)))
    (loop for i from 2 below limit do
	 (loop for j from i below limit by i do
	      (incf (aref v j))))
    (loop for i from 2 below (1- limit) count (= (aref v i) (aref v (1+ i))))))
