(in-package :project-euler)

(defun possible-ways-to-sum-with-generating-fun (n table)
  (if (< n 0)
      0
      (if (> (elt table n) 0)
	  (elt table n)
	  (setf (elt table n)
		(let ((pn 0))
		  (loop for k from 1 to (isqrt n) do
		       (let ((n1 (- n (/ (* k (1- (* 3 k))) 2)))
			     (n2 (- n (/ (* k (1+ (* 3 k))) 2))))
			 (let ((pn1 (possible-ways-to-sum-with-generating-fun n1 table))
			       (pn2 (possible-ways-to-sum-with-generating-fun n2 table)))
			   (if (oddp k)
			       (incf pn (+ pn1 pn2))
			       (decf pn (+ pn1 pn2))))))
		  pn)))))

(defun problem-78 ()
  (let ((table (make-array '(60000))))
    (setf (elt table 0) 1)
    (find-if #'(lambda (i) (= 0 (mod (possible-ways-to-sum-with-generating-fun i table) 1000000)))
	     (loop for i from 3 to 60000 collect i))))