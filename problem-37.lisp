(in-package :project-euler)
(defun can-do-full-match (where-to-match what-to-match)
  (cond ((= 0 (length what-to-match)) t)
	((= 0 (length where-to-match)) nil)
	(t (or (and (= (car where-to-match) (car what-to-match)) 
		    (can-do-full-match (cdr where-to-match) (cdr what-to-match)))
	       (can-do-full-match (cdr where-to-match) what-to-match)))))

(defun find-full-match (current-match what-to-match)
  (if (can-do-full-match current-match what-to-match)
      (if (= 0 (length current-match))
	  what-to-match
	  current-match)))
	    
  
(defun digits-1-3-7-9 (number)
  (let ((digits (number-to-digits number)))
    (every  #'(lambda (x) x)
	    (loop for digit in '(0 2 4 5 6 8) collect (notany #'(lambda (x) (= digit x)) digits)))))

(defun right-truncatable-with-sieve (digits sieve)
  (let ((current-number 0)
	(result t))
    (loop for digit in digits do
	  (setf current-number (+ (* 10 current-number) digit))
	  (when (not (prime-from-sieve-p current-number sieve))
	    (setf result nil)
	    (return)))
    result))

(defun left-truncatable-with-sieve (digits sieve)
  (let ((current-number 0)
	(result t)
	(scaler 1))
    (loop for digit in (reverse digits) do
	  (incf current-number (* scaler digit))
	  (setf scaler (* 10 scaler))
	  (when (not (prime-from-sieve-p current-number sieve))
	    (setf result nil)
	    (return)))
    result))

(defun next-1-3-7-9-number-with-carry (n)
  (let ((next-digit '((1 . 3 ) (3 . 7) (7 . 9) (9 . 1))))
    (if (= 1 (length n))
    (values (list (cdr (assoc (car n) next-digit))) (= 9 (car n)))
    (multiple-value-bind (result carry) (next-1-3-7-9-number-with-carry (cdr n))
      (if carry
          (values (cons (cdr (assoc (car n) next-digit)) result) (= 9 (car n)))
          (values (cons (car n) result) nil))))))

(defun next-1-3-7-9-number (n)
  (multiple-value-bind (result carry) (next-1-3-7-9-number-with-carry n)
    (if carry
	(cons 1 result)
	result)))


(defun problem-37 (max)
  (let ((the-sieve (make-prime-sieve max))
	(sum 0)
	(num-primes 0))
    (loop for i = 23 then (1+ i) until (or (> i max) (= num-primes 11)) do
	  (when (prime-from-sieve-p i the-sieve) 
	    (let ((the-digits (number-to-digits i)))
	      (when (and (right-truncatable-with-sieve the-digits the-sieve)
			 (left-truncatable-with-sieve the-digits the-sieve))
		(incf sum i)
		(incf num-primes)))))
    (list :sum sum :num-primes num-primes)))
