
(in-package :project-euler)

(defun truncate-n-digits (a number-of-digits)
  (rem a (expt 10 number-of-digits)))

(defun truncated-* (a b number-of-digits)
  (truncate-n-digits (* a b) number-of-digits))

(defun truncated-square (a number-of-digits)
  (truncate-n-digits (* a a) number-of-digits))

(defun truncated-expt (a b number-of-digits)
  (cond ((or (zerop b) (= 1 b)) (truncate-n-digits a number-of-digits))
	((zerop (mod b 2)) (truncated-square (truncated-expt a (floor b 2) number-of-digits) number-of-digits))
	(t (truncated-* (truncated-square (truncated-expt a (floor b 2) number-of-digits) number-of-digits) a number-of-digits))))

(defun truncated-tetration (a b number-of-digits)
  (loop for c from 1 to b 
     and result = a then (truncated-expt result a number-of-digits)
       finally (return result)))

(defun truncated-tetration-s (a b number-of-digits)
  (cond ((= 1 b) (truncate-n-digits a number-of-digits))
	(t (truncated-expt a (truncated-tetration a (1- b) number-of-digits) number-of-digits))))

(defun tetration (a b)
  (if (= b 1) 
      a
      (expt a (tetration a (1- b)))))
      
(defun problem-188 (&optional (a 1777) (b 1855) (number-of-digits 8))
  (truncated-tetration a b number-of-digits))


(defun reduce-expt-a-b-mod-p (a p)
  "returns q so that a^b mod p = 1"
  (loop for number = (* a a) then (mod (* number a) p)
       for i from 1 until (= number a)
       finally (return i)))
  

(defun expt-mod (power-tower p)
  (if (null (rest power-tower))
      (mod (first power-tower) p)
      (let ((q (reduce-expt-a-b-mod-p (first power-tower) p)))
	(format t "->~a~%" q)
	(expt-mod (rest power-tower) q))))
  

;; a^b mod p =a^(b mod q) mod p

;; 1777^^1855 mod 10^8 = 1777^(1777^^1854) mod 10^8 = 1777^(1777^^1854 mod 1250000) mod 10^8
;; 1777^^1854 mod 1250000 = 1777^(1777^^1853) mod 1250000 = 1777^(1777^^1853 mod 62500) mod 