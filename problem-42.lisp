(in-package :project-euler)
(defun get-problem-42-data ()
  (read-problem-n-data 42))

(defun problem-42 ()
  (let ((word-values (sort (mapcar #'word-value (get-problem-42-data)) #'<))
	(triangle-hash (make-hash-table)))
    (mapcar #'(lambda (n) (setf (gethash (/ (* n (+ n 1)) 2) triangle-hash) t)) (range 1 20))
    (loop for value in word-values summing (if (gethash value triangle-hash) 1 0))))
    
