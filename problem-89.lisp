(in-package :project-euler)
(defun best-number-to-roman (number)
  (if (> number 3999)
      (concatenate 'string "M" (best-number-to-roman (- number 1000)))
      (format nil "~@R" number)))

(defun roman-digit-value (roman-digit-char)
  (let ((roman-digit-values 
	 '((#\I . 1) (#\V . 5) (#\X . 10) (#\L . 50) (#\C . 100) (#\D . 500) (#\M . 1000))))
    (cdr (assoc roman-digit-char roman-digit-values))))
  
(defun read-roman-numeral-from-string (roman-string)
  (let ((value 0) (prevvalue 0) (sum 0))
  (loop for index from (1- (length roman-string)) downto 0 do
	(setf value (roman-digit-value (elt roman-string index)))
	(if (>= value prevvalue)
	    (incf sum value)
	    (decf sum value))
	(setf prevvalue value))
  sum))

(defun problem-89 ()
  (let* ((roman-numbers (read-problem-n-data 89))
	 (roman-strings (mapcar #'symbol-name roman-numbers))
	 (roman-values (mapcar #'read-roman-numeral-from-string roman-strings))
	 (best-roman-strings (mapcar #'best-number-to-roman roman-values)))
    (- (length (apply #'concatenate 'string roman-strings))
       (length (apply #'concatenate 'string best-roman-strings)))))
	 

