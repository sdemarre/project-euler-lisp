(in-package :project-euler)
(defun problem-25 (&optional (num-digits 1000))
  (do ((count 1 (1+ count))
       (a 0 b)
       (b 1 (+ a b))
       (digits 1 (round (+ 0.5 (log (+ a b) 10)))))
      ((>= digits num-digits) count)
    (format t "~a~%" digits)))

