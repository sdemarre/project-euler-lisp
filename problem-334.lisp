(in-package :project-euler)
(defun do-partial-334-step (bowls position other-bowls)
  (when (> (elt bowls position) 1)
    (when (= position (1- (length bowls)))
      (vector-push-extend 0 bowls))
    (let ((left-pos (if (zerop position)
			(cons other-bowls 0)
			(cons bowls (1- position))))
	  (right-pos (cons bowls (1+ position))))
      (decf (elt bowls position) 2)
      (incf (elt (car left-pos) (cdr left-pos)))
      (incf (elt (car right-pos) (cdr right-pos))))
    t))

(defun do-334-step (down-bowls up-bowls position up-bowls-p)
  (if up-bowls-p
      (do-partial-334-step up-bowls position down-bowls)
      (do-partial-334-step down-bowls position up-bowls)))

(defun display-bowls (down-bowls up-bowls)
  (iter (for index from (1- (length down-bowls)) downto 0)
	(format t "~a " (elt down-bowls index)))
  (format t "| ")
  (iter (for index from 0 to (1- (length up-bowls)))
	(format t "~a " (elt up-bowls index))))

(defun reset-334 ()
  (setf *down-bowls* (make-array '(1) :initial-element 0 :adjustable t :fill-pointer t))
  (setf *up-bowls* (make-array '(1) :initial-element 0 :adjustable t :fill-pointer t))
  (setf (elt *down-bowls* 0) 10)
  (setf (elt *up-bowls* 0) 9))

(defun d ()
  (display-bowls *down-bowls* *up-bowls*)
  (format t "~%"))

(defun s (n)
  (if (>= n 0)
      (do-334-step *down-bowls* *up-bowls* n t)
      (do-334-step *down-bowls* *up-bowls* (- -1 n) nil)))

(defun find-candidate-positions (down-bowls up-bowls)
  (append (iter (for i from 0 to (1- (length down-bowls)))
		(when (> (elt down-bowls i) 1)
		  (collect (- -1 i))))
	  (iter (for i from 0 to (1- (length up-bowls)))
		(when (> (elt up-bowls i) 1)
		  (collect i)))))

(defun count-334-steps ()
  (reset-334)
  (let ((candidate-positions (find-candidate-positions *down-bowls* *up-bowls*))
	(steps 0))
    (iter (while (not (null candidate-positions)))
	  (s (alexandria:random-elt candidate-positions))
	  (incf steps)
	  (setf candidate-positions (find-candidate-positions *down-bowls* *up-bowls*)))
    steps))


(defun p-334-t (i)
  (if (zerop i)
      123456
      (let ((t-i-m1 (p-334-t (1- i))))
	(if (evenp t-i-m1)
	    (/ t-i-m1 2)
	    (logxor (/ (1- t-i-m1) 2) 926252)))))

(defun p-334-b (i)
  (1+ (mod (p-334-t i) (expt 2 11))))


(defun count-output-bowls (beans)
  "count the number of output bowls required for full expansion of a bowl with beans beans"
  (declare (fixnum beans))
  (the fixnum (1+ (* 2 (floor beans 2)))))

(defun count-steps-for-full-expansion (beans)
  "count the number of steps required for full expansion of a bowl with beans beans,
  which is actually \sum_{i=1}^{\floor{beans/2}}i^2"
  (let ((n2 (floor beans 2)))
    (* 1/6 n2 (1+ n2) (1+ (* 2 n2)))))

(defun bowls-required-for-next-step (bowls)
  "number of bowls required for fully expanded beans for each of the input bowls"
  (declare (type (simple-vector) bowls))
  (let ((minpos 0)
	(maxpos (1- (length bowls))))
    (iter (for beans in-vector bowls with-index idx)
	  (let ((out-bowls-limit (/ (1- (count-output-bowls beans)) 2)))
	    (setf minpos (min minpos (- idx out-bowls-limit)))
	    (setf maxpos (max maxpos (+ idx out-bowls-limit)))))
    (1+ (- maxpos minpos))))

(defun output-bowls-offset (bowls)
  (let ((minpos 0))
    (iter (for beans in-vector bowls with-index idx)
	  (setf minpos (min minpos (- idx (/ (1- (count-output-bowls beans)) 2)))))
    (- minpos)))

(defun add-next-step-beans (beans output-bowls center-bowl-index)
  (declare (type (simple-vector) output-bowls))
  (let ((out-bowls-half (ash beans -1)))
    (iter (for idx from (- center-bowl-index out-bowls-half) to  (+ center-bowl-index out-bowls-half))
	  (incf (svref output-bowls idx))))
  (if (evenp beans)
      (decf (svref output-bowls center-bowl-index)))
  output-bowls)
	     
  
(defun compute-next-334-step (bowls)
  "input: vector with bowl counts.
output: (values next-step-vector number-steps-taken)"
  (declare (type (simple-vector) bowls))
  (let ((output-bowls (make-array (list (bowls-required-for-next-step bowls)) :initial-element 0))
	(output-bowls-offset (output-bowls-offset bowls))) ;; output-bowls-offset is the position of the center of bowl[0] in output-bowls
    (iter (for beans in-vector bowls with-index idx)
	  (add-next-step-beans beans output-bowls (+ output-bowls-offset idx)))
    (values output-bowls (reduce #'+ bowls :key #'count-steps-for-full-expansion))))
	
(defun expansion-done (bowls)
  (declare (type (simple-vector) bowls))
  (notany #'(lambda (n) (> n 1)) bowls))

(defun new-334-steps (bowls)
  (let ((steps 0))
    (iter (while (not (expansion-done bowls)))
	  (multiple-value-bind (new-bowls new-steps) (compute-next-334-step bowls)
	    (incf steps new-steps)
	    (setf bowls new-bowls)))
    steps))
 
