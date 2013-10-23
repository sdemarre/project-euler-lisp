(in-package :project-euler)

(defun sum-of-divisors (number)
  (let ((prod 1)
	(n number))
    (do ((k 2 (+ k 1)))
	((<= (* k k) n) prod)
      (let ((p 1))
	(while (= 0 (mod n k))
	  (setf p (+ 1 (* p k)))
	  (setf n (floor n k)))
	(setf prod (* p prod))))
    (when (> n 1)
      (setf prod (* prod (+ 1 n))))
    prod))

(defun abundant-number-p (number)
  (> (reduce #'+ (divisors number)) number))

(defun problem-23 ()
  (let ((abundant-numbers (remove-if-not #'abundant-number-p (range 12 28123)))
	(number-array (make-array 28124 :initial-element nil)))
    (loop for first in abundant-numbers do
	  (loop for second in abundant-numbers do
		(let ((sum (+ first second)))
		  (when (> sum 28123)
		    (return))
		  (setf (aref number-array sum) t))))
    (loop for i from 28123 downto 0 when (not (aref number-array i)) sum i)))
