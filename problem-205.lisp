(in-package :project-euler)

(defun probability-to-throw (number-of-dice dice-type sum &optional (distribution (n-dm-distribution number-of-dice dice-type)))
  (if (or (< sum number-of-dice) (> sum (* number-of-dice dice-type)))
      0
      (distribution-full-info-p-equal (find-if #'(lambda (info) (= sum (distribution-full-info-value info))) distribution))))


(defun n-dm-distribution (number-of-dice dice-type)
  "give n dice with dice-type eyes, returns a list of (value number-of-ways-to-have-that-value)"
  (let ((distrib (power-poly (loop for i from 1 to dice-type collect 1) number-of-dice)))
    (loop for sum = number-of-dice then (1+ sum) 
       for number in distrib 
       collect (list sum number))))

(defun distribution-to-cumulative (distribution)
  "given a (value number-of-values) list, returns a list of (value number-of-ways-to-have-less-than-or-equal-value)"
  (let ((sum 0))
    (mapcar #'(lambda (l) (list (first l) (incf sum (second l)))) distribution)))

(defun distribution-to-inverse-cumulative (distribution)
  "given a (value number-of-values) list, returns a list of (value number-of-ways-to-have-bigger-than-value)"
  (let ((sum (reduce #'+ (mapcar #'second distribution))))
    (mapcar #'(lambda (l) (list (first l) (decf sum (second l)))) distribution)))

(defstruct distribution-full-info
  value
  p-smaller
  p-equal
  p-larger)

(defmethod print-object ((object distribution-full-info) s)
  (print-unreadable-object (object s :type t)
    (format s ":v ~a :< ~a := ~a :> ~a" 
	    (distribution-full-info-value object)
	    (distribution-full-info-p-smaller object)
	    (distribution-full-info-p-equal object)
	    (distribution-full-info-p-larger object))))  
  
(defun distribution-to-full-info (distribution)
  (let* ((sum (reduce #'+ (mapcar #'second distribution)))
	 (running-sum 0)
	 (total-sum sum))
    (mapcar #'(lambda (l) (make-distribution-full-info :value (first l) 
						       :p-smaller (/ (prog1 running-sum (incf running-sum (second l))) total-sum)
						       :p-equal (/ (second l) total-sum)
						       :p-larger (/ (decf sum (second l)) total-sum)))
	    distribution)))

(defun find-in-distribution (value distribution extractor)
  (let ((info (find-if #'(lambda (full-info) (= (distribution-full-info-value full-info) value)) distribution)))
    (if info
	(funcall extractor info)
	0)))

(defun probability-to-win (sum number-dice dice-type &optional (distribution (distribution-to-full-info (n-dm-distribution number-dice dice-type))))
  (find-in-distribution sum distribution #'distribution-full-info-p-larger))

(defun probability-to-equal (sum number-dice dice-type &optional (distribution (distribution-to-full-info (n-dm-distribution number-dice dice-type))))
  (find-in-distribution sum distribution #'distribution-full-info-p-equal))

(defun probability-to-lose  (sum number-dice dice-type &optional (distribution (distribution-to-full-info (n-dm-distribution number-dice dice-type))))
  (find-in-distribution sum distribution #'distribution-full-info-p-smaller))

(defun possible-sums (number-dice dice-type)
  (loop for i from number-dice to (* number-dice dice-type) collect i))


(defun problem-205 (&optional (n1 9) (d1 4) (n2 6) (d2 6))
  (let* ((n1-d1-distribution (distribution-to-full-info (n-dm-distribution n1 d1)))
	 (n2-d2-distribution (distribution-to-full-info (n-dm-distribution n2 d2))))
    (loop for d1-sum from n1 to (* n1 d1) summing
	 (* (probability-to-throw n1 d1 d1-sum n1-d1-distribution) (probability-to-lose d1-sum n2 d2 n2-d2-distribution)))))

