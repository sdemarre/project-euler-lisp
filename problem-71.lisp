(in-package :project-euler)
(defun problem-71 ()
  (cadr (sort (loop for i from (* 2 (/ 999999 7)) to (* 3 (/ 999999 7)) collect (/ i 999999)) #'>)))
