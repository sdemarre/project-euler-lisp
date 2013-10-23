(in-package :project-euler)

(defun count-digits (number)
  (1+ (floor (log number 10))))
(defun power-10-mask (number)
  (expt 10 (count-digits number)))
(defun number-ends-with (number end-number)
  (= (mod number (power-10-mask end-number)) end-number))
(defun number-starts-with (number start-number)
  (= (floor number (expt 10 (- (count-digits number) (count-digits start-number)))) start-number))
(defun combine-numbers (n1 n2)
  (+ (* n1 (power-10-mask n2)) n2))
(defun all-number-combinations (numbers)
  (loop for a on numbers append
       (loop for b on (rest a) append
	    (list (combine-numbers (first a) (first b)) (combine-numbers (first b) (first a))))))
       
(defun can-add-to-list (numbers new-number)
  (and (every #'(lambda (n) (cllib:primep (combine-numbers n new-number))) numbers)
       (every #'(lambda (n) (cllib:primep (combine-numbers new-number n))) numbers)))

(defun extend-p-60-list (numbers list-to-chose-from &optional (start-from 3))
  (let ((start-value (apply #'max  (cons start-from numbers))))
    (let ((possible-extension (find-if #'(lambda (x) (and (> x start-value) (can-add-to-list numbers x))) list-to-chose-from)))
      (if possible-extension
	  (append numbers (list possible-extension))
	  numbers))))
  
(defun do-maximum-p-60-extension (start-value list-to-chose-from)
  (do* ((current-list (list start-value) extended-list)
       (extended-list (extend-p-60-list current-list list-to-chose-from) (extend-p-60-list current-list list-to-chose-from)))
      ((= (length current-list) (length extended-list)) current-list)))

(defun try-all-possible-p-60-extensions (list-to-chose-from)
  (loop for start-value in list-to-chose-from when (>= start-value 3) do
       (let ((extension (do-maximum-p-60-extension start-value list-to-chose-from)))
	 (if (= 4 (length extension))
	     (format t "~%~a->~a~%" extension (reduce #'+ extension))
	     (format t "~a~%" start-value)))))

  
(let ((max-value-init 26033))
  (defclass p-60-partial-solution (simple-solver::partial-solution)
    ((partial-solution :initform nil)
     (max-value :initform max-value-init)
     (primes :initform (remove-if #'(lambda (n) (or (< n 12) (> n max-value-init))) (rest (cllib:primes-to max-value-init)))))))

   
(defmethod simple-solver::is-solution ((partial-solution p-60-partial-solution))
  (= 5 (length (slot-value partial-solution 'partial-solution))))

(defmethod simple-solver::possible-extensions ((partial-solution p-60-partial-solution))
  (let ((last-element (first (slot-value partial-solution 'partial-solution))))
    (if (not last-element)
	(slot-value partial-solution 'primes)
	(loop for result on (slot-value partial-solution 'primes) do
	     (when (> (first result) last-element)
	       (return result))))))

(defmethod simple-solver::can-extend-solution ((partial-solution p-60-partial-solution) possible-extension)
  (or (null (slot-value partial-solution 'partial-solution))
      (and (<= (+ (reduce #'+ (slot-value partial-solution 'partial-solution))
	   	 (* (- 5 (length (slot-value partial-solution 'partial-solution))) possible-extension))
	      (slot-value partial-solution 'max-value))
	   (can-add-to-list (slot-value partial-solution 'partial-solution) possible-extension))))

(defmethod simple-solver::extend-solution ((partial-solution p-60-partial-solution) possible-extension)
  (setf (slot-value partial-solution 'partial-solution) (push possible-extension (slot-value partial-solution 'partial-solution))))
  ;;(format t "  extended to ~a~%" (slot-value partial-solution 'partial-solution)))

(defmethod simple-solver::unextend-solution ((partial-solution p-60-partial-solution) possible-extension)
  (setf (slot-value partial-solution 'partial-solution) (rest (slot-value partial-solution 'partial-solution))))
;  (format t "unextended to ~a~%" (slot-value partial-solution 'partial-solution)))

(defmethod simple-solver::handle-solution ((partial-solution p-60-partial-solution))
  (format t "found solution?~a->~a~%" (slot-value partial-solution 'partial-solution) (reduce #'+ (slot-value partial-solution 'partial-solution))))

