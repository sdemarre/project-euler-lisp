(in-package :project-euler)
(defun rotations (number)
  (let ((digits (coerce (number-to-digits number) 'vector)))
    (loop for i from 0 to (1- (length digits))
	  collect (let ((rotation 0))
		    (loop for idx from 0 to (1- (length digits)) do
			  (setf rotation  (+ (elt digits (mod (+ i idx) (length digits))) (* 10 rotation))))
		    rotation))))

(defun make-prime-sieve (last-number)
  (let ((the-sieve (make-array (+ last-number 1) :initial-element t)))
    (mapcar #'(lambda (x) (setf (elt the-sieve x) nil)) '(0 1))
    (loop for i from 2 to (sqrt last-number) do
	  (let ((idx (* 2 i)))
	    (while (<= idx last-number)
	      (setf (elt the-sieve idx) nil)
	      (incf idx i))))
    the-sieve))

(defun prime-from-sieve-p (number sieve)
  (elt sieve number))

(defun problem-35 (&optional (limit 1000000))
  (length (let ((the-sieve (make-prime-sieve limit)))
	    (flet ((prime-p (x) (prime-from-sieve-p x the-sieve)))
	      (loop for i from 2 to limit 
		 when (and (prime-p i) (not (position nil (mapcar #'prime-p (rotations i))))) collect i)))))
