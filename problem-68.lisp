(in-package :project-euler)

(defstruct 5-gon
  (positions (make-array '(10))))

(defmacro sum-5-gon-elements (5-gon n1 n2 n3)
  `(+ (svref ,5-gon ,n1) (svref ,5-gon ,n2) (svref ,5-gon ,n3)))
(defun 5-gon-line-sum (5-gon line)
  "given a 5-gon and a number from 0 to 4 indicating the starting external node, return the sum of the nodes on that line"
  (declare (type fixnum line)
	   (type simple-vector 5-gon))
  (cond ((= 0 line) (sum-5-gon-elements 5-gon 0 1 2))
	((= 1 line) (sum-5-gon-elements 5-gon 3 2 4))
	((= 2 line) (sum-5-gon-elements 5-gon 5 4 6))
	((= 3 line) (sum-5-gon-elements 5-gon 7 6 8))
	((= 4 line) (sum-5-gon-elements 5-gon 9 8 1))))

(defmacro 5-gon-elements-smaller (5-gon index1 index2)
  `(< (svref ,5-gon ,index1) (svref ,5-gon ,index2)))
(defun correctly-ordered-5-gon-p (5-gon)
  (and (5-gon-elements-smaller 5-gon 0 3)
       (5-gon-elements-smaller 5-gon 0 5)
       (5-gon-elements-smaller 5-gon 0 7)
       (5-gon-elements-smaller 5-gon 0 9)))

(defun sum-5-gon-digits (5-gon)
  (reduce #'combine-numbers (mapcar #'(lambda (idx) (elt 5-gon idx)) '(0 1 2 3 2 4 5 4 6 7 6 8 9 8 1))))


(defun is-magical-5-gon-p (5-gon)
  (and (apply #'= (mapcar #'(lambda (index) (5-gon-line-sum 5-gon index)) '(0 1 2 3 4)))
       (correctly-ordered-5-gon-p 5-gon)))


(defun problem-68 ()
  (let ((max-result 0))
    (with-permutations-swap (5-gon (vector 1 2 3 4 5 6 7 8 9 10))
      (when (and (is-magical-5-gon-p 5-gon)
		 (let ((digit-sum (sum-5-gon-digits 5-gon)))
		   (and (= 16 (number-of-digits digit-sum))
			(> digit-sum max-result))))
	(setf max-result (sum-5-gon-digits 5-gon))))
    max-result))
