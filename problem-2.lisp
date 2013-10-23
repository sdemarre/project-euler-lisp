(in-package :project-euler)
(defun evenfib (n)
  (let ((f (fib n)))
    (if (evenp f) f 0)))

(defun problem-2 (&optional (max 4000000))
  (do ((n 0 (1+ n))
       (sum 0 (+ sum (evenfib n))))
      ((< max (fib n)) sum)))
