(in-package :project-euler)
(defun partitions (number max)
  "construct a list of lists, all possible partitions of number, without using numbers larger than max"
  (cond ((zerop number) (list nil))
    ((zerop max) (list nil))
    (t (loop for first from (min number max) downto 1 append
         (mapcar #'(lambda (rest) (cons first rest)) (partitions (- number first) (min max first)))))))

(defun parallel-capacitors (cap1 cap2)
  (+ cap1 cap2))

(defun serial-capacitors (cap1 cap2)
  (/ (* cap1 cap2) (+ cap1 cap2)))

(defun combine-capacitor-values (number1 number2 capacity)
  (remove-duplicates (loop for cap1 in (capacitor-values number1 capacity) append
               (loop for cap2 in (capacitor-values number2 capacity) append
                 (list (serial-capacitors cap1 cap2) (parallel-capacitors cap1 cap2))))))

(defun capacitor-values (number &optional (cap1 60))
  (cond ((= number 1) (list cap1))
    ((= number 2) (list (serial-capacitors cap1 cap1) (parallel-capacitors cap1 cap1)))
    (t (remove-duplicates (let ((possible-sums (cdr (cllib:number-sum-split number #'identity #'identity nil))))
			    (loop for i in possible-sums append (combine-capacitor-values (car i) (cdr i) cap1)))))))

(defun vector-of-sets-get-values (vec index)
  (loop for k being the hash-keys of (aref vec index) collect k))

(defun vector-of-sets-add-value (vec index value)
  (setf (gethash value (aref vec index)) 1))

(defun compute-num-capacitor-values (num-capacitors possible-values)
  (macrolet ((get-values (index) `(vector-of-sets-get-values possible-values ,index))
	     (add-value (index value) `(vector-of-sets-add-value possible-values ,index ,value)))
    (let ((possible-sums (cdr (cllib:number-sum-split num-capacitors #'identity #'identity nil))))
      (loop for sum in possible-sums do
	    (loop for cap1 in (get-values (car sum)) do
		  (loop for cap2 in (get-values (cdr sum)) do
			(add-value num-capacitors (serial-capacitors cap1 cap2))
			(add-value num-capacitors (parallel-capacitors cap1 cap2))))))))

(defun problem-155 (num-capacitors)
  (let ((possible-values (make-array (1+ num-capacitors) :fill-pointer 0))
	(capacity 60))
    (loop for i from 0 to num-capacitors do (setf (aref possible-values i) (make-hash-table)))
    (macrolet ((get-values (index) `(vector-of-sets-get-values possible-values ,index))
	       (add-value (index value) `(vector-of-sets-add-value possible-values ,index ,value)))
      (add-value 1 capacity)
      (add-value 2 (parallel-capacitors capacity capacity))
      (add-value 2 (serial-capacitors capacity capacity))
      (loop for i from 3 to num-capacitors do
	    (compute-num-capacitor-values i possible-values))
      (loop for i from 1 to (1- num-capacitors) do
	    (loop for cap in (get-values i) do
		  (add-value num-capacitors cap)))
      (get-values num-capacitors))))
