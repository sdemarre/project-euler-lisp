(in-package :project-euler)
(defun number-digits-iter (n l)
  (if (= n 0)
      l
      (multiple-value-bind (quotient remainder) (floor n 10)
	(number-digits-iter quotient (cons remainder l)))))

(defun digits-power-sum (number power)
  (let* ((digits (number-to-digits number))
	 (power-digits (mapcar #'(lambda (x) (expt x power)) digits)))
  (apply #'+ power-digits)))

(defun problem-30 (power)
  (let ((result nil))
    (dotimes (i 250000)
      (when (= i (digits-power-sum i power))
	(setf result (cons i result))))
    (1- (reduce #'+ result))))
