(in-package :project-euler)
(defun get-triangle-data (num)
  (coerce (mapcar #'(lambda (x) (setf x (coerce x 'vector))) (read-problem-n-data num)) 'vector))

(defun get-problem-18-data ()
  (get-triangle-data 18))

(defun get-problem-67-data ()
  (get-triangle-data 67))

(defun problem-triangle (num)
  (let ((data (get-triangle-data num)))
    (macrolet ((element (row column) `(elt (elt data ,row) ,column)))
      (loop for row from 1 to (1- (length data)) do
	    (loop for column from 0 to row do
		  (cond ((= 0 column) (incf (element row column) (element (1- row) column)))
			((= row column) (incf (element row row) (element (1- row) (1- row))))
			(t (incf (element row column)
				 (max (element (- row 1) column) (element (1- row) (1- column))))))))
      (reduce #'max (elt data (1- (length data)))))))

(defun problem-18 ()
  (problem-triangle 18))

(defun problem-67 ()
  (problem-triangle 67))
