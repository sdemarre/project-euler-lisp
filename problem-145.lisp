(in-package :project-euler)
(defun oddly-reversible-p (number)
  (if (= 0 (mod number 10))
      nil
      (let ((sum (+ number (reverse-number number))))
	(every #'oddp (number-digits sum)))))

(defun problem-145 (max)
  (let ((num 0))
    (loop for i from 1 to max do (when (oddly-reversible-p i) (incf num)))
    num))
