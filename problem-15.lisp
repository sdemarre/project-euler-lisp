(in-package :project-euler)
(defun problem-15 (&optional (size-x 20) (size-y 20))
  (let ((cache (make-array (list (+ size-x 1) (+ size-y 1)) :initial-element nil)))
    (macrolet ((get-array (x y) `(aref cache ,x ,y)))
      (setf (get-array 0 0) 0)
      (loop for m from 0 to size-x do
	    (loop for n from 0 to size-y do
		  (if (or (= m 0) (= n 0))
		      (setf (get-array m n) 1)
		      (setf (get-array m n) (+ (get-array (1- m) n) (get-array m (1- n)))))))
      (get-array size-x size-y))))

(defun problem-15-bis (&optional (size-x 20) (size-y 20))
  (number-combinations size-x (+ size-x size-y)))