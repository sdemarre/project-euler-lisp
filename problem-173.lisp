(in-package :project-euler)

(defun number-tiles-for-laminae (outer-side-length number-laminae)
  (* 4 number-laminae (- outer-side-length number-laminae)))

(defun problem-173 (&optional (limit 1000000))
  (let ((total 0)
	(start-length 3))
    (loop for number-laminae = 1 then (+ 1 number-laminae)
       until (> (number-tiles-for-laminae start-length number-laminae) limit)
       do
	 (format t "number-laminae = ~a~%" number-laminae)
	 (loop for length = start-length then (1+ length)
	    until (> (number-tiles-for-laminae length number-laminae) limit)	    
	    do 
	      (format t "  length = ~a, number tiles = ~a~%" length (number-tiles-for-laminae length number-laminae))
	      (incf total))
	 (incf start-length 2))
    total))

(defun problem-173-fast (&optional (limit 1000000))
  (let ((max-number-laminae (- (floor (1+ (sqrt (+ (* 2 limit) 9.0))) 2) 2)))
    (format t "max-number-laminae = ~a~%" max-number-laminae)
    (loop for number-laminae from 1 to max-number-laminae 
	 summing (progn
		   (format t " number-laminae = ~a~%" number-laminae)
		   (- (floor (+ limit (* 4 (* number-laminae number-laminae))) (* 4 number-laminae)) (+ 3 number-laminae -1) -1)))))

