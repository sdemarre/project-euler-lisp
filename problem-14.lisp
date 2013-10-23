(in-package :project-euler)
(defun collatz-step (l)
  (let ((n (car l)))
    (if (= 1 n)
	l
	(if (evenp n)
	  (cons (/ n 2) l)
	  (cons (+ (* 3 n) 1) l)))))

(defun collatz-list (n)
  (do ((result (list n) (collatz-step result)))
      ((= 1 (car result)) result)))


(defun collatz-length (n)
  (do ((len 1 (1+ len))	(val n (if (evenp val) (/ val 2) (+ (* 3 val) 1))))
      ((= val 1) len)))

(defun collatz-length-recursive (n)
  (if (= n 1)
      1
      (if (evenp n)
	  (+ 1 (collatz-length-recursive (/ n 2)))
	  (+ 1 (collatz-length-recursive (+ (* 3 n) 1))))))

(defun problem-14 (&optional (limit 1000000))
  (let ((maxfound 0))
    (loop for i from 1 to limit do 
	  (let ((len (collatz-length i)))
	  (when (> len maxfound)
	    (setf maxfound len)
	    (format t "new max: i = ~a, length = ~a~%" i len))))))
