(in-package :project-euler)
(defun number-to-binary (number &optional (width 0))
  (let ((result nil)
	(width width))
    (do ((tmp number (floor tmp 2)))
	((and (<= width 0) (= tmp 0)) result)
      (push (mod tmp 2) result)
      (decf width))))

(defun binary-palindrome-p (number)
  (let ((bin (number-to-binary number)))
    (equalp bin (reverse bin))))

(defun problem-36 (&optional (number 1000000))
  (loop for i from 1 to number when (and (oddp i) (binary-palindrome-p i) (number-palindrome-p i)) summing i))
