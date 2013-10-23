(in-package :project-euler)

(defun num-permutations-that-are-cubes (cubes i)
  (loop for obj in cubes counting (= (cdr obj) (cdr i))))

(defun find-lowest-with-perms (hash num-permutations)
  (loop for key being the hash-keys of hash
	when (= (car (gethash key hash)) num-permutations) minimizing (cdr (gethash key hash))))

(defun problem-62 (num-permutations)
  (flet ((canonical-cube (x) 
	   (let ((cube (* x x x))) 
	     (cons cube (vector-digits-to-number (sort (number-to-digits-vector cube 20) #'>))))))
    (let ((hash (make-hash-table)))
      (do ((i 1 (1+ i)) (last-num-perms 0))
	  ((= last-num-perms (1+ num-permutations)) (find-lowest-with-perms hash num-permutations))
	(let* ((data (canonical-cube i)))
	  (if (gethash (cdr data) hash)
	      (setf last-num-perms (incf (car (gethash (cdr data) hash))))
	      (setf (gethash (cdr data) hash) (cons 1 (car data)))))))))
