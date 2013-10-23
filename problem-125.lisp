(in-package :project-euler)

(defun problem-125-debug (&optional (limit (expt 10 8)))
  (let* ((isqrt-limit (isqrt limit))
	 (squares (loop for i from 1 to isqrt-limit collect (* i i)))
	 (current-values (copy-seq squares))
	 (sum 0)
	 (number-palindromes 0)
	 (level 2)
	 (results))
    (loop until (null current-values) do
	 (setf current-values 
	       (loop for square in squares for current in (rest current-values) until (> (+ square current) limit) collect (+ square current)))
	 (loop for value in current-values when (number-palindrome-p value) do
	      (let ((start (1+ (position value current-values))))
		(push (list value (range start (1- (+ start level)))) results))
	      (incf sum value)
	      (incf number-palindromes))
	 (incf level))
    (list sum number-palindromes results)))

(defun problem-125 (&optional (limit (expt 10 8)))
  (let* ((isqrt-limit (isqrt limit))
	 (squares (loop for i from 1 to isqrt-limit collect (* i i)))
	 (current-values (copy-seq squares))
	 (palindromes))
    (loop until (null current-values) do
	 (setf current-values 
	       (loop for square in squares for current in (rest current-values) until (> (+ square current) limit) collect (+ square current)))
	 (loop for value in current-values when (number-palindrome-p value) do
	      (pushnew value palindromes)))
    (reduce #'+ palindromes)))
	 

(defun sum-of-squares (numbers)
  (reduce #'(lambda (x y) (+ x (* y y))) numbers :initial-value 0))