(in-package :project-euler)
(defun increasing-number-p (digits)
  (let ((start #\0))
    (not (loop for char across digits do
	       (when (char> char start)
		 (setf start char))
	       (when (char< char start)
		 (return t))))))
	
(defun decreasing-number-p (digits)
  (let ((start #\9))
    (not (loop for char across digits do
	       (when (char< char start)
		 (setf start char))
	       (when (char> char start)
		 (return t))))))

(defun bouncy-number-p (n)
  (let ((digits (prin1-to-string n)))
    (not (or (increasing-number-p digits) (decreasing-number-p digits)))))

(defun problem-112 (&optional (goal-percentage 99))
  (let ((bouncy-count 0))
    (loop for number = 1 then (1+ number)
      do
      (when (bouncy-number-p number)
        (incf bouncy-count))
      (when (= goal-percentage (/ (* 100 bouncy-count) number))
        (return number)))))

