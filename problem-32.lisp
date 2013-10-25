(in-package :project-euler)
(defun number-partitions-with-cache (n cache)
  (cond	((zerop n) 1)
	((<= n 3) n)
	(t (let ((value (elt cache n)))
	     (if value
		 value
		 (setf (elt cache n) 
		       (loop for k from 1 to (isqrt n) summing
			     (let* ((a1 (- n (* (/ 1 2) k (1- (* 3 k)))))
				    (a2 (- n (* (/ 1 2) k (1+ (* 3 k)))))
				    (p1 (if (< a1 0) 0 (number-partitions-with-cache a1 cache)))
				    (p2 (if (< a2 0) 0 (number-partitions-with-cache a2 cache))))
			       (* (expt -1 (1+ k)) (+ p1 p2))))))))))


(defun number-partitions (n)
  (let ((cache (make-array (1+ n) :initial-element nil)))
    (loop for i from 1 to (1- n) do
	  (number-partitions-with-cache i cache))
    (number-partitions-with-cache n cache)))


(defun add-splits-to-set (digits hash)
  (loop for a from 1 to 3 do
	(loop for b from 1 to (/ (- 9 a) 2) do
	      (let* ((factor1 (vector-digits-to-number (subseq digits 0 a)))
		     (factor2 (vector-digits-to-number (subseq digits a (+ a b))))
		     (product (vector-digits-to-number (subseq digits (+ a b)))))
		(when (= (* factor1 factor2) product)
		  (format t "found ~a x ~a = ~a~%" factor1 factor2 product)
		  (setf (gethash product hash) t))))))
		
(defun problem-32 ()
  (let ((products (make-hash-table)))
    (with-permutations-swap (permutation #(1 2 3 4 5 6 7 8 9))
      (add-splits-to-set permutation products))
    (loop for product being the hash-keys of products summing product)))
