(in-package :project-euler)

(defun figurate-functions ()
  (list #'triangle-number
	#'square-number
	#'pentagonal-number
	#'hexagonal-number
	#'heptagonal-number
	#'octogonal-number))

(defun get-4-digit-numbers (f)
  "list of 4-digit f-numbers"
  (loop for i = 1 then (1+ i) until (> (funcall f i) 9999)
     when (< 999 (funcall f i)) collect (funcall f i)))

(defun figurate-4-digit-numbers ()
  "hash of f->list of 4-digit f-numbers"
  (let ((figurate-numbers (make-hash-table)))
    (loop for f in (figurate-functions) do
	 (setf (gethash f figurate-numbers) (get-4-digit-numbers f)))
    figurate-numbers))

(defun digits-ok-for-cycle (number1 number2)
  (and (> (remainder number1 100) 9)
       (= (remainder number1 100) (floor number2 100))))

(defun compute-next-cycle-elements (element figurate-hash)
  (loop for f being the hash-keys of figurate-hash collect
       (loop for next-element in (gethash f figurate-hash)
	  when (digits-ok-for-cycle element next-element)
	    collect (list f next-element))))

(defun compute-possible-next-elements-hash ()
  "returns a hash of hashes, function -> (number -> possible next cycle elements for every figurate f)"
  (let ((result (make-hash-table :test #'eq))
	(figurate-hash (figurate-4-digit-numbers)))
    (loop for f in (figurate-functions) do
	 (setf (gethash f result) (make-hash-table))
	 (loop for element in (gethash f figurate-hash) do
	      (setf (gethash element (gethash f result))
		    (compute-next-cycle-elements element figurate-hash))))
    result))

(defun extend-cycles (list-of-cycles possible-next-elements-hash)
  (let (new-cycles)
    (loop for cycle in list-of-cycles do
	 (let ((f (first (first cycle)))
	       (value (second (first cycle))))
	   (loop for possible-next-elements in (gethash value (gethash f possible-next-elements-hash))
	      for current-f in (figurate-functions)
	      when possible-next-elements do
		(loop for possible-next-element in possible-next-elements do
		     (push (cons possible-next-element cycle) new-cycles)))
	   (when (every #'null (gethash value (gethash f possible-next-elements-hash)))
	     (push cycle new-cycles))))
    new-cycles))

(defun possible-cycles (start-element possible-next-elements-hash)
  (let (list-of-cycles)
    (loop for f in (figurate-functions)
       when (gethash start-element (gethash f possible-next-elements-hash))
       do (push (list (list f start-element)) list-of-cycles))
    (loop for step from 2 to 6 do
	 (setf list-of-cycles (extend-cycles list-of-cycles possible-next-elements-hash)))
    list-of-cycles))

(defun compute-all-cycles ()
  (let ((possible-starts (reduce #'union (mapcar #'get-4-digit-numbers (figurate-functions))))
	(possible-next-elements-hash (compute-possible-next-elements-hash))
	(cycles (make-hash-table)))
    (loop for start in possible-starts do
       (setf (gethash start cycles) (possible-cycles start possible-next-elements-hash)))
    cycles))

(defun find-possible-cycles ()
  (let ((cycles (compute-all-cycles)))
    (loop for start being the hash-keys of cycles collect
       (loop for cycle in (gethash start cycles) collect (length cycle)))))

(defun possible-extensions (number possible-next-elements-hash)
  (reduce #'append
	  (reduce #'append
		  (loop for f being the hash-keys of possible-next-elements-hash 
		     when (gethash number (gethash f possible-next-elements-hash))
		     collect (gethash number (gethash f possible-next-elements-hash))))))
	    
    
(defun compute-possible-extensions ()
  (let ((result (make-hash-table))
	(possible-starts (reduce #'union (mapcar #'get-4-digit-numbers (figurate-functions))))
	(possible-next-elements-hash (compute-possible-next-elements-hash)))
    (loop for start in possible-starts do
	 (setf (gethash start result) (possible-extensions start possible-next-elements-hash)))
    result))

(defun compute-possible-starts ()
  (reduce #'union (mapcar #'get-4-digit-numbers (figurate-functions))))

(defun cyclic-p (cycle)
  (digits-ok-for-cycle (second (first cycle)) (second (car (last cycle)))))

(defun has-all-figurate-functions-p (cycle)
  (every #'(lambda (f) (assoc f cycle)) (figurate-functions)))

(defun problem-61 ()
  (let ((the-cycles 
	 (let ((possible-next-elements-hash (compute-possible-next-elements-hash)))
	   (loop for start in (compute-possible-starts) collect
		(let ((cycle-candidates (possible-cycles start possible-next-elements-hash)))
		  (loop for cycle in cycle-candidates 
		     when (and (= 6 (length cycle))
			       (cyclic-p cycle)
			       (has-all-figurate-functions-p cycle))
		     collect cycle))))))
    (reduce #'+ (mapcar #'second (caar (remove-if-not #'identity the-cycles))))))

