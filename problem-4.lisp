(in-package :project-euler)
(defun problem-4 ()
  (let ((result-list (loop for i from 100 to 999 collect
			   (loop for j from 100 to 999 collect 
				 (let ((product (* i j)))
				   (if (number-palindrome-p product) `(,i ,j ,product)))))))
    (car (sort (remove-if-not #'identity (apply #'append result-list)) #'(lambda (x y) (> (nth 2 x) (nth 2 y)))))))

