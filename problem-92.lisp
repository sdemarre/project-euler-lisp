(in-package :project-euler)
(defun sum-of-digit-squares (number)
  (loop for i = number then (truncate i 10) until (zerop i) 
       summing (let ((r (rem i 10))) (* r r))))

(defun end-of-chain (number)
  (loop for sum = (sum-of-digit-squares number)
	then (sum-of-digit-squares sum)
	until (or (= sum 1) (= sum 89))
	finally (return sum)))

(defun problem-92 (&optional (number 10000000))
  (let ((the-ends (coerce (mapcar #'end-of-chain (range 1 600)) 'vector)))
    (loop for i from 1 to number
	  counting (= 89 (elt the-ends (1- (sum-of-digit-squares i)))))))
