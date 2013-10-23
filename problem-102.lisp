(in-package :project-euler)
(defun point-part-of-triangle (x y triangle)
  (macrolet ((fill-in (x y x1 y1 x2 y2) `(- (* (- ,y2 ,y1) (- ,x ,x1)) (* (- ,y ,y1) (- ,x2 ,x1)))))
    (destructuring-bind (x0 y0 x1 y1 x2 y2) triangle
      (let ((fillz-0-1 (fill-in x  y  x0 y0 x1 y1))
	    (fillz-0-2 (fill-in x  y  x0 y0 x2 y2))
	    (fillz-1-2 (fill-in x  y  x1 y1 x2 y2))
	    (fillp-0-1 (fill-in x2 y2 x0 y0 x1 y1))
	    (fillp-0-2 (fill-in x1 y1 x0 y0 x2 y2))
	    (fillp-1-2 (fill-in x0 y0 x1 y1 x2 y2)))
	(or (zerop fillz-0-1) (zerop fillz-0-2) (zerop fillz-1-2)   ;point is on one of the lines
	    (and (= (signum fillz-0-1) (signum fillp-0-1))
		 (= (signum fillz-0-2) (signum fillp-0-2))
		 (= (signum fillz-1-2) (signum fillp-1-2))))))))

(defun problem-102 ()
  (let ((data (read-problem-n-data 102)))
    (loop for triangle in data count (point-part-of-triangle 0 0 triangle))))
