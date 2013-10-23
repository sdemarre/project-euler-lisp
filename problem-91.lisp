(in-package :project-euler)

(defun compute-distance-square (point1 point2)
  (let ((d1 (- (first point1) (first point2)))
	(d2 (- (second point1) (second point2))))
    (+ (* d1 d1) (* d2 d2))))
(defun are-right-angle-triangle-distances (dist1 dist2 dist3)
  (let ((big (max dist1 dist2 dist3)))
    (= big (apply #'+ (remove big (list dist1 dist2 dist3))))))
(defun is-right-angle-triangle (point1 point2 point3)
  (and point1 
       point2
       point3
       (let ((distance-a (compute-distance-square (first point1) (first point2)))
	     (distance-b (compute-distance-square (first point1) (first point3)))
	     (distance-c (compute-distance-square (first point2) (first point3))))
	 (are-right-angle-triangle-distances distance-a distance-b distance-c))))
	     
	       
(defun problem-91 (&optional (size 50))
  (let ((points (loop for x from 0 to size append
		     (loop for y from 0 to size collect (list x y))))
	(point1 '((0 0))))
    (loop for point2 on (rest points) summing
	 (loop for point3 on (rest point2) count (is-right-angle-triangle point1 point2 point3)))))
		   