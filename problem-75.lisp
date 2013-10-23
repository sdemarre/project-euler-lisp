(in-package :project-euler)


(defun coprime-p (m n)
  (= 1 (gcd m n)))

(defun gen-a-b-c (m n)
  (let ((m2 (* m m))
	(n2 (* n n)))
    (list (- n2 m2) (* 2 m n) (+ m2 n2))))

(defun add-solution (solutions m n)
  (if (coprime-p m n)
      (let* ((abc (gen-a-b-c m n))
	     (length (reduce #'+ abc)))
      (if (or (evenp m) (evenp n))	  
	  (push abc (gethash length solutions nil))
	  (push (mapcar #'(lambda (x) (/ x 2)) abc) (gethash (/ 2 length) solutions nil))))))

(defun problem-75 (&optional (limit 1500000))
  (let ((solutions (make-hash-table)))
    (iter (for m from 1)
	  (until (if (< limit (+ (* 4 m m) (* 2 m))) (prog1 t (format t "m=~a~%" m)) nil))  ; n2-m2   2nm  n2+m2  with n=1+m
	  (iter (for n from (1+ m))
		(let ((length (* 2 n (+ n m))))
		  (if (< limit (* 2 n (+ n m)))
		      (return)
		      (add-solution solutions m n)))))
    solutions))
		  

(defun show-values (l)
  (destructuring-bind (m n) l
    (let ((m2 (* m m))
	  (n2 (* n n)))
      (let ((a (- n2 m2))
	    (b (* 2 n m))
	    (c (+ m2 n2)))
	(list (list a b c (+ a b c))
	      (list (* a a) (* b b) (* c c)))))))
