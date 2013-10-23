(in-package :project-euler)
(defun problem-29 ()
  (let ((the-hash (make-hash-table))
	(num-entries 0))
    (loop for a from 2 to 100 do
	  (loop for b from 2 to 100 do
		(setf (gethash (expt a b) the-hash) 1)))
    (loop for k being the hash-keys in the-hash do (incf num-entries))
    num-entries))
