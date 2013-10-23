(in-package :project-euler)

(defun power-count-multiplies (b)
  (if (or (zerop b) (= 1 b)) 0
      (if (evenp b)
	  (+ 1 (power-count-multiplies (/ b 2)))
	  (+ 2 (power-count-multiplies (/ (1- b) 2))))))
      