(in-package :project-euler)


(defun f-114 (row-length tile-length cache)
  (if (= -1 row-length) 
      1
      (cond-with-cache row-length tile-length cache
		       ((= -1 row-length) 1)
		       ((zerop tile-length) 1) ;; (format t "tile-length = 0~%")
		       ((< row-length tile-length) 1) ;;(format t "row-length < tile-length~%")   ;; don't tile
		       ((= row-length tile-length) 2) ;;(format t "row-length = tile-length~%")    ;; don't tile or single tile
		       (t (1+ (loop for new-tile-length from tile-length to row-length summing
				   (loop for position from 0 to (- row-length new-tile-length) summing
					(f-114 (- row-length position new-tile-length 1) tile-length cache))))))))


(defun problem-114 (&optional (l 50) (n 3))
  (let ((cache (make-array `(,(1+ l) ,(1+ l)) :initial-element nil)))
    (f-114 l n cache)))