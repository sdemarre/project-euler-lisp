(in-package :project-euler)
(defun get-problem-13-data ()
  (read-problem-n-data 13))

(defun problem-13 ()
  (reduce #'+ (get-problem-13-data)))
