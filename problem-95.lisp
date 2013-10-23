(in-package :project-euler)

(defun compute-chain-data (chain-data idx)
  (labels ((ccd-rec (chain-data idx length min-value initial-idx)
	     (cond ((= idx 1) ;; getting to 1 means we don't have a cycle
		    (cons 0 0))
		   ((= idx initial-idx) (cons (1+ length) (min min-value (aref chain-data idx))))
		   (t (ccd-rec chain-data (aref chain-data idx) (1+ length) (min min-value (aref chain-data idx)) initial-idx)))))
    (ccd-rec chain-data (aref chain-data idx) 1 idx idx)))
(defun problem-95 (max)
  (let* ((raw-chain-data (iter (for i from 1 to max)
			       (collect (reduce #'+ (fast-divisors i)))))
	 (chain-data (coerce (mapcar #'(lambda (x) (if (> x max) 1 x)) raw-chain-data) 'vector)))    
    (let ((best-chain-length 0)
	  (best-chain-min-value 0))
      (iter (for idx from 2 to max)
	    (destructuring-bind (chain-length . min-value) (compute-chain-data chain-data idx)
	      (when (> chain-length best-chain-length)
		(setf best-chain-length chain-length)
		(setf best-chain-min-value min-value))))
      best-chain-min-value)))