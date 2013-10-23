(in-package :project-euler)

(defun reverse-and-add (number)
  (+ (reverse-number number) number))

(defun iters-before-palindrome (number max-iters)
  (let ((iters 0)
	(current-number (+ number (reverse-number number))))
    (while (and (not (number-palindrome-p current-number)) (< iters max-iters))
      (incf current-number (reverse-number current-number))
      (incf iters))
    iters))

(defun problem-55 ()
  (loop for i from 1 to 10000 counting (> (iters-before-palindrome i 100) 50)))
