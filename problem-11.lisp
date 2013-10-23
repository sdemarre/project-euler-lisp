(in-package :project-euler)
(let (data)
  (defun get-problem-11-data ()
    (if data
	data
	(setf data (read-problem-n-data 11)))))

(defun get-element (x y)
  (aref (get-problem-11-data) x y))

(defun can-get-horizontal (x y)
  (declare (ignore y))
  (< x 17))
(defun can-get-vertical (x y)
  (declare (ignore x))
  (< y 17))
(defun can-get-diagonal-nwse (x y)
  (and (can-get-horizontal x y) (can-get-vertical x y)))
(defun can-get-diagonal-nesw (x y)
  (and (> x 3) (< x 20) (< y 17)))

(defun get-horizontal (x y)
  (mapcar #'(lambda (i) (get-element (+ i x) y)) (list 0 1 2 3)))
(defun get-vertical (x y)
  (mapcar #'(lambda (i) (get-element x (+ i y))) (list 0 1 2 3)))
(defun get-diagonal-nwse (x y)
  (mapcar #'(lambda (i) (get-element (+ i x) (+ i y))) (list 0 1 2 3)))
(defun get-diagonal-nesw (x y)
  (mapcar #'(lambda (i) (get-element (- x i) (+ i y))) (list 0 1 2 3)))

(defun get-horizontal-product (x y)
  (if (can-get-horizontal x y)
      (reduce #'* (get-horizontal x y))
      0))
(defun get-vertical-product (x y)
  (if (can-get-vertical x y)
      (reduce #'* (get-vertical x y))
      0))
(defun get-diagonal-nwse-product (x y)
  (if (can-get-diagonal-nwse x y)
      (reduce #'* (get-diagonal-nwse x y))
      0))
(defun get-diagonal-nesw-product (x y)
  (if (can-get-diagonal-nesw x y)
      (reduce #'* (get-diagonal-nesw x y))
      0))

(defun problem-11-debug ()
  (loop for x from 0 to 19 collect
	(loop for y from 0 to 19 collect 
	      (list x y 
		    (get-horizontal-product x y) (get-vertical-product x y)
		    (get-diagonal-nwse-product x y) (get-diagonal-nesw-product x y)))))

(defun problem-11 ()
  (loop for x from 0 to 19 maximize
	(loop for y from 0 to 19 maximize 
	      (max (get-horizontal-product x y)
		   (get-vertical-product x y)
		   (get-diagonal-nwse-product x y)
		   (get-diagonal-nesw-product x y)))))
