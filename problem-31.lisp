(in-package :project-euler)
(defun get-possible-coin-selections (value coins)
  (cond ((= value 0) (loop repeat (length coins) collect 0))
	((endp coins) nil)
	(t (loop for num-coins from (floor value (car coins)) downto 0
		 appending (mapcar #'(lambda (x) (cons num-coins x))
				   (get-possible-coin-selections (- value (* num-coins (car coins))) (cdr coins)))))))

(defun problem-31 (&optional (amount 200))
  (let ((coins '(1 2 5 10 20 50 100 200 500)))
    (length (remove-duplicates (get-possible-coin-selections amount coins) :test #'equal))))


(let ((number-coins 8)
      (amount-to-split 201))
  (defun problem-31-dynamic ()
    (let ((denom #(1 2 5 10 20 50 100 200))
	  (arr (make-array (list amount-to-split number-coins) :initial-element 1)))
      (loop for i from 1 below amount-to-split do
	   (loop for j from 1 below number-coins do
		(setf (aref arr i j) (if (>= (- i (svref denom j)) 0)
					 (+ (aref arr i (1- j)) (aref arr (- i (svref denom j)) j))
					 (aref arr i (1- j))))))
      (aref arr (1- amount-to-split) (1- number-coins)))))
		    
		
    