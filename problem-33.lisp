(in-package :project-euler)

(defun problem-33 ()
  (let ((product 1))
    (loop for numer from 11 to 99 do
	  (loop for denom from (+ 1 numer) to 99 do
		(loop for digit in (number-to-digits numer) do
		      (if (and (/= digit 0) (find digit (number-to-digits denom)))
			  (let ((simple-numer (digits-to-number (remove digit (number-to-digits numer))))
				(simple-denom (digits-to-number (remove digit (number-to-digits denom)))))
			    (when (and (/= 0 simple-denom) (= (/ numer denom) (/ simple-numer simple-denom)))
			      (format t "found ~a/~a or ~a/~a~%" numer denom simple-numer simple-denom)
			      (setf product (* product (/ numer denom)))))))))
    product))

