(in-package :project-euler)
(defmacro factorials-vector (number)
  (cons 'vector (mapcar #'fact (range 0 number))))

(let ((facts (factorials-vector 9)))
  (defun digits-fact-sum (number)
    (reduce #'+ (mapcar #'(lambda (x) (aref facts x)) (number-digits number)))))

(defun problem-74-non-repeating-chain-length (number cache)
  (let ((hash (make-hash-table))
	(step-count 1)
	(repeat-found nil)
	(sum (digits-fact-sum number)))
    (if (not (zerop (aref cache sum)))
	(aref cache sum)
	(progn
	  (setf (gethash sum hash) step-count)
	  (until repeat-found
	    (incf step-count)
	    (setf sum (digits-fact-sum sum)) 
	    (setf repeat-found (gethash sum hash))
	    (setf (gethash sum hash) step-count))
	  (loop for key being the hash-keys of hash do
		(setf (aref cache key) (1+ (- step-count (gethash key hash)))))
	  step-count))))

(defun problem-74 ()
  (let ((cache (make-array (1+ (digits-fact-sum 999999)) :initial-element 0)))
    (loop for i from 1 to 999999 counting (= 60 (problem-74-non-repeating-chain-length i cache)))))
