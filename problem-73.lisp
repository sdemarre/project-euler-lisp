(in-package :project-euler)
(defun problem-73 ()
  (let ((hash (make-hash-table))) 
    (loop for denom from 1 to 10000 do
	  (loop for numer from 1 to denom
		when (and (< (/ numer denom) (/ 1 2)) (> (/ numer denom) (/ 1 3))) do
		(setf (gethash (/ numer denom) hash) 1)))
    (loop for k being the hash-key of hash summing 1)))
