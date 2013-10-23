(in-package :project-euler)
(defun is-square (number)
  (= number (expt (isqrt number) 2)))

(defun decompose-christian-goldbach (composite-number)
  (loop for i from (1- composite-number) downto 2 do
	(when (and (prime-p i)
		   (evenp (- composite-number i)) 
		   (is-square (/ (- composite-number i) 2)))
	  (return (list i (- composite-number i) (isqrt (/ (- composite-number i) 2)))))))

(defun problem-46 ()
  (let ((composite-number 9))
    (while (decompose-christian-goldbach composite-number)
      ;(format t "can decompose ~a = ~a~%" composite-number (decompose-christian-goldbach composite-number))
      (incf composite-number 2)
      (while (prime-p composite-number)
	(incf composite-number 2)))
    composite-number))
