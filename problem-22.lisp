(in-package :project-euler)
(defun get-problem-22-data ()
  (read-problem-n-data 22))

(defun letter-value (letter)
  (1+ (position letter "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :test #'char=)))

(defun word-value (word)
  (loop for c across word summing (letter-value c)))

(defun problem-22 ()
  (let ((the-names (copy-list (get-problem-22-data))))
    (setf the-names (sort the-names #'string<))
    (loop for name in the-names 
	  for pos from 1 to (length the-names)
	  summing (* pos (word-value name)))))

