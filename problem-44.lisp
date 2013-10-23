(in-package :project-euler)
(defun is-func-pentagonal (func pentagonal-numbers penta-hash i j)
  (gethash (funcall func (elt pentagonal-numbers i) (elt pentagonal-numbers j)) penta-hash))

(defun is-sum-pentagonal (pentagonal-numbers penta-hash i j)
  (is-func-pentagonal #'+ pentagonal-numbers penta-hash i j))

(defun is-difference-pentagonal (pentagonal-numbers penta-hash i j)
  (is-func-pentagonal #'- pentagonal-numbers penta-hash i j))

(defun problem-44 ()
  (let* ((max 5000)
	 (pentagonal-numbers (coerce (mapcar #'pentagonal-number (range 0 max)) 'vector))
	 (penta-hash (make-hash-table :size max)))
    (map 'vector #'(lambda (x) (setf (gethash x penta-hash) t)) pentagonal-numbers)
    (loop for i from 2 to max append
	  (loop for j from 1 to (1- i)
		when (and (is-difference-pentagonal pentagonal-numbers penta-hash i j)
			  (is-sum-pentagonal pentagonal-numbers penta-hash i j))
		collect (list :i i :j j)))))
