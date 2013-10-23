(in-package :project-euler)
(defun radical (number)
  (apply #'* (remove-duplicates (ifactor number))))

(defun problem-124 ()
  (let* ((unsorted (loop for i from 1 to 100000 collect (list i (radical i))))
     (sorted (sort unsorted #'(lambda (x y) (if (= (cadr x) (cadr y)) (< (car x) (car y)) (< (cadr x) (cadr y)))))))
    (nth 9999 sorted)))
