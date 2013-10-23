(in-package :project-euler)
(defun problem-85-num-rectangles (m n)
  (/ (* (* (1+ n) n) (* (1+ m) m)) 4))

(defun problem-85 (&optional (target-surface 1000000))
  (let ((min-distance target-surface)
	(min-pair nil))
    (loop for m from 1 to (* 2 (sqrt target-surface)) do
	  (loop for n from 1 to (* 2 (sqrt target-surface)) do 
		(let ((surface-difference (abs (- (problem-85-num-rectangles m n) target-surface))))
		  ;(format t "looking at (~a, ~a)->~a, diff = ~a~%" m n (problem-85-num-rectangles m n) surface-difference)
		  (when (< surface-difference min-distance)
		    (setf min-distance surface-difference)
		    (setf min-pair (list m n))))))
    (values min-distance min-pair (* (nth 0 min-pair) (nth 1 min-pair)))))

