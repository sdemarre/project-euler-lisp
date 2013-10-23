(in-package :project-euler)
(defun check-problem-38-properties (number)
  (let ((result '()))
    (loop for multiplier = 1 then (1+ multiplier) until (>= (length result) 9) do
	 (append result (number-to-digits (* multiplier number))))
    (when (and (= 9 (length result))
	     (list-is-1-9-pandigital result))
      (format t "~a~%" result))))
	  
(defun problem-38 ()
  (loop for number from 1 to 9 do
       (check-problem-38-properties number))
  (loop for number from 12 to 98 do
       (check-problem-38-properties number))
  (loop for number from 123 to 987 do
       (check-problem-38-properties number))
  (loop for number from 1234 to 9876 do
       (check-problem-38-properties number)))
