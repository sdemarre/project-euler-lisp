(in-package :project-euler)

(defun get-problem-81-data ()
  (read-problem-n-data 81))

(defun find-lowest-path-sum (original-data)
  (let* ((data (coerce (mapcar #'(lambda (x) (coerce x 'vector)) original-data) 'vector))
	 (max-index (1- (length data))))
    (macrolet ((get-data (x y) `(elt (elt data ,y) ,x)))
      (loop for x from 0 to max-index do
	    (loop for y from 0 to max-index do
		  (let ((min-path-sum (cond ((and (= x 0) (= y 0)) 0)
					    ((= x 0) (get-data x (1- y)))
					    ((= y 0) (get-data (1- x) y))
					    (t (min (get-data (1- x) y) (get-data x (1- y)))))))
		    (incf (get-data x y) min-path-sum))))
      (get-data max-index max-index))))

(defun problem-81 ()
  (find-lowest-path-sum (get-problem-81-data)))
