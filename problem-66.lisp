(in-package :project-euler)

(defun solve-pell (d)
  "solves x^2-Dy^2=1 for a specified d, with x and y integers.
 returns (cons x y) with x and y the minimal solution or nil if no solution exists."
  (let ((s (isqrt d)))
    (if (= (* s s) d)
	nil
	(let ((continued-fraction (continued-fraction-for-sqrt d)))
	  (if (oddp (length continued-fraction))
	      (let ((convergent (continued-fraction-sum (butlast continued-fraction))))
		(cons (numerator convergent) (denominator convergent)))
	      (let* ((doubled-continued-fraction (append continued-fraction (rest continued-fraction) (list (second continued-fraction))))
		     (convergent (continued-fraction-sum doubled-continued-fraction)))
		(cons (numerator convergent) (denominator convergent))))))))

(defun number-of-combinations (n p)
  (/ (fact n) (* (fact (- n p)) (fact p))))

(defun make-pell-solver (d)
  "returns (lambda (p) (p-th solution for x^2-Dy^2=1))"
  (destructuring-bind (p . q) (solve-pell d)
    (lambda (n)
      (cons
       (loop for i = n then (- i 2) until (< i 0)
	  summing (* (number-of-combinations n i) (expt p i) (expt q (- n i)) (expt d (/ (- n i) 2))))
       (loop for i = (1- n) then (- i 2) until (< i 0)
	  summing (* (number-of-combinations n i) (expt p i) (expt q (- n i)) (expt d (/ (- (1- n) i) 2))))))))
    
    
    

(defun test-pell (pell-solution d)
  (- (* (car pell-solution) (car pell-solution)) (* d (cdr pell-solution) (cdr pell-solution))))