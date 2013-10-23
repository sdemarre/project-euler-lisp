(in-package :project-euler)

(defun do-334-step (bowls start-position)
  (let ((beans (svref bowls start-position))
	(number-bowls (length bowls)))
    (unless (zerop (svref bowls start-position))
      (setf (svref bowls start-position) 0)
      (iter (for i from 1 to beans)
	    (incf (svref bowls (mod (+ i start-position) number-bowls)))))
    (values bowls (mod (+ start-position beans) number-bowls))))

(defun all-ones-p (bowls)
  (every #'(lambda (x) (= 1 x)) bowls))

(defun count-334-steps (number-bowls)
  (let ((bowls (make-array (list number-bowls) :initial-element 1))
	(start-position 0)
	(steps 0))
    (iter (multiple-value-bind (v p) (do-334-step bowls start-position)
	    (declare (ignorable v))
	    (incf steps)
	    (setf start-position p))
	  (until (all-ones-p bowls)))
    steps))
