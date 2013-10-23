(in-package :project-euler)

(defun fact (x)
  (if (< x 2)
      1
      (do ((i 1 (1+ i))
	   (result 1 (* result i)))
	  ((< x i) result))))

(defun fib (x)
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= x n) cur)))

(defun smallest-factor (n)
  (let ((result (loop for i from 2 to (isqrt n) do (when (= 0 (mod n i)) (return i)))))
    (if result result n)))

(defun prime-p (n)
  (when (not (= 1 n))
      (= (smallest-factor n) n)))

(defun partial-factor (l)
  (let* ((sfcl (smallest-factor (first l))))
    (if (= sfcl (first l))
	l
	(cons (floor (first l) sfcl) (cons sfcl (cdr l))))))

(defun ifactor-iter (l)
  (let ((pf (partial-factor l)))
    (if (equalp pf l) l (ifactor-iter pf))))

(defun ifactor (n)
  (ifactor-iter (list n)))

(defun ifactors (n)
  (ifactor-iter (list n)))

(defun number-palindrome-p (n)
  (= n (reverse-number n)))

(defun square (x) (* x x))
(defun cube (x) (* x x x))
(defun expt4 (x) (square (square x)))

(defun find-next-prime (n)
    (do ((tmp (+ n 1) (1+ tmp))) ((prime-p tmp) tmp)))

(defun digit-sum (n)
  (do ((tmp n (floor tmp 10))
       (sum 0 (+ sum (mod tmp 10))))
      ((= tmp 0) sum)))

(defun num-combinations (n r)
  (/ (fact n) (* (fact (- n r)) (fact r))))

(defun get-nth-permutation (number l)
  "nth lexicographical permutation. starts counting at 0, i.e. if number is 0, l will be returned"
  (if (= 1 (length l))
      (if (= number 0) 
	  l
	  (error "impossible!!"))
      (let ((perm-count (fact (1- (length l)))))
	(multiple-value-bind (num-perms rem) (floor number perm-count)
	  (append (list (nth num-perms l)) (get-nth-permutation rem (remove (nth num-perms l) l)))))))

(defun range (start stop)
  (loop for i from start to stop collect i))

(defun selected-products (selectors numbers)
  (if (or (null selectors) (null numbers))
      1
      (* (expt (first numbers) (first selectors)) (selected-products (rest selectors) (rest numbers)))))
(defparameter *prime-vector* nil)
(defun init-prime-vector (max)
  (setf *prime-vector* (make-prime-list-for-range max)))
(defun is-prime-from-tab (n)
  (unless (and *prime-vector* (<= n (1- (length *prime-vector*))))
    (init-prime-vector (floor (* n 1.4))))
  (aref *prime-vector* n))
(defun extract-factors (n i)
  (let ((power 0)
	(n n))
    (iter (while (zerop (mod n i)))
	  (incf power)
	  (setf n (floor n i)))
    (cons power n)))
(defun primes-list-to (n)
  (is-prime-from-tab n)			; init prime tab
  (iter (for i from 2 to n)
	(when (aref *prime-vector* i)
	  (collect i))))
(defun prime-factors (n)
  (labels ((prime-factors-rec (factor-candidates n current)
	     (cond ((null factor-candidates)
		    (if (= 1 n)
			current
			(cons (cons n 1) current)))
		   ((zerop (mod n (car factor-candidates)))
		    (destructuring-bind (power . remainder) (extract-factors n (car factor-candidates))
		      (prime-factors-rec (rest factor-candidates)
					 remainder
					 (cons (cons (car factor-candidates) power) current))))
		   (t (prime-factors-rec (rest factor-candidates) n current)))))
    (prime-factors-rec (primes-list-to (isqrt n)) n nil)))
(defun expanded-prime-factors (n)
  (let ((prime-factors (prime-factors n)))
    (iter (for (factor . power) in prime-factors)
	  (appending (iter (for i from 1 to power)
			   (collect factor))))))
(defun divisors (n)
  (iter (for i from 1 to (floor n 2))
	(when (zerop (mod n i))
	  (collect i))))
(defun make-selected-product (selector numbers)
  "returns the product of numbers, but only for the elements of numbers for which the bit in selectors is 1"
  (selected-products (reverse (number-to-binary selector)) (reverse numbers)))
(defun fast-divisors (number)
  "returns a list of the divisors of number. e.g.: 220->(1 2 4 5 10 11 20 22 44 55 110)"
  (let ((prime-factors (expanded-prime-factors number)))
    (remove-duplicates (loop for i from 0 below (1- (expt 2 (length prime-factors)) )
			  collect (make-selected-product i prime-factors)))))
(defun vector-digits-to-number (digits)
  (let ((result 0))
    (loop for digit across digits do (setf result (+ (* 10 result) digit)))
    result))
(defun number-to-digits-vector (n &optional (initial-vector-size 10))
  (let ((result (make-array initial-vector-size :fill-pointer 0 :adjustable t)))
    (loop for char across (format nil "~d" n) do (vector-push-extend (- (char-code char) (char-code #\0)) result))
    result))

(defun number-of-digits (number)
  (1+ (floor (log number 10))))

(defun eval-poly (coefficients x)
  "evaluate a polynomial with coefficients at x"
  (reduce #'(lambda (result coeff) (+ (* result x) coeff)) coefficients :initial-value 0))


(defun poly-horner-form (var coefs)
  (cond ((null coefs)       0)
	((null (rest coefs)) (first coefs))
	(t
	 `(+ ,(first coefs) (* ,var ,(poly-horner-form var (rest coefs)))))))

(defmacro eval-horner-form (var coeffs)
  (poly-horner-form var coeffs))
(defun add-polys (poly1 poly2)
  (let ((npoly1 poly1)
	(npoly2 poly2)
	(l-poly1 (length poly1))
	(l-poly2 (length poly2)))
    (cond ((> l-poly1 l-poly2) (dotimes (i (- l-poly1 l-poly2)) (push 0 npoly2)))
	  (t (dotimes (i (- l-poly2 l-poly1)) (push 0 npoly1))))
    (loop for coeff1 in npoly1 for coeff2 in npoly2 collect (+ coeff1 coeff2))))

(defun shift-poly-left (poly &optional (powers 1))
  (append poly (loop for i from 0 to (1- powers) collect 0)))

(defun scale-poly (poly scale)
  (loop for coeff in poly collect (* scale coeff)))

(defun multiply-polys (poly1 poly2)
  (let* ((l-poly1 (length poly1))
	 (l-poly2 (length poly2))
	 (npoly1 (if (< l-poly1 l-poly2) poly1 poly2))
	 (npoly2 (if (< l-poly1 l-poly2) poly2 poly1))
	 (scaled-polys (loop for coeff in npoly1 collect (scale-poly npoly2 coeff))))
    (reduce #'(lambda (poly1 poly2) (add-polys (shift-poly-left poly1) poly2)) scaled-polys :initial-value '())))

(defun power-poly (poly power)
  (cond ((zerop power) '(1))
	((= 1 power) poly)
	((evenp power) (power-poly (multiply-polys poly poly) (/ power 2)))
	(t (multiply-polys poly (power-poly poly (1- power))))))
	 
(defun interpolating-poly (datapoints)
  (let* ((first-order-polys (mapcar #'(lambda (datapoint) (list 1 (- 0 (first datapoint)))) datapoints))
	 (nth-order-polys (loop for datapoint in datapoints for point-index = 0 then (1+ point-index) collect
			       (loop for poly in first-order-polys for poly-index = 0 then (1+ poly-index) collect
				    (if (= poly-index point-index) '(1) poly))))
	 (expanded-polys (mapcar #'(lambda (nth-order-poly) (reduce #'multiply-polys nth-order-poly :initial-value '(1))) nth-order-polys))
	 (scaled-polys (loop for expanded-poly in expanded-polys for datapoint in datapoints collect 
			    (scale-poly expanded-poly (/ (second datapoint) (eval-poly expanded-poly (first datapoint)))))))
    (reduce #'add-polys scaled-polys)))
	 
    
(defun number-combinations (number-to-chose total-to-chose-from)
  (/ (fact total-to-chose-from) (fact number-to-chose) (fact (- total-to-chose-from number-to-chose))))


(defun make-prime-list-for-range (maximum)
    (let ((result-array (make-array (list (1+ maximum)) :initial-element t)))
      ;; init for 0, 1
      (setf (aref result-array 0) nil)
      (setf (aref result-array 1) nil)
      ;; process the rest
      (loop
         for base-num from 2 below (1+ (/ maximum 2))
         do
           (let ((n (* base-num 2)))
             (loop
                (if (<= n maximum)
                    (progn
                      (setf (aref result-array n) nil)
                      (incf n base-num))
                    (return)))))
      result-array))
