(in-package :project-euler)
(defun get-problem-99-data ()
  (read-problem-n-data 99))

(defun problem-99 ()
  (flet ((elt-size (l) (* (nth 1 l) (log (nth 0 l)))))
    (let ((data (sort (copy-seq (get-problem-99-data)) 
		      #'(lambda (x y) (> (elt-size x) (elt-size y))))))
      (car data))))
