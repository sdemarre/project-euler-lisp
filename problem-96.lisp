(in-package :project-euler)

(defclass sudoku-digit ()
  ((digit :accessor digit :initform 0)
   (possible-values  :initform (make-array (list 10) :initial-element 1))))

(defmethod initialize-instance :after ((digit sudoku-digit) &key)
  (setf (aref (slot-value digit 'possible-values) 0) 0))

(defgeneric possible-value-p (digit value))
(defmethod possible-value-p ((digit sudoku-digit) value)
  (not (zerop (aref (slot-value digit 'possible-values) value))))
(defgeneric possible-values (digit))
(defmethod possible-values ((digit sudoku-digit))
  (loop for i from 1 to 9 when (possible-value-p digit i) collect i))

(defgeneric add-possible-value (digit possible-value))
(defmethod add-possible-value ((digit sudoku-digit) possible-value)
  (setf (aref (slot-value digit 'possible-values) possible-value) 1))

(defgeneric remove-possible-value (digit possible-value))
(defmethod remove-possible-value ((digit sudoku-digit) possible-value)
  (setf (aref (slot-value digit 'possible-values) possible-value) 0))

(defmethod number-possible-values (digit))
(defmethod number-possible-values ((digit sudoku-digit))
  (loop for i from 1 to 9 count (not (zerop (aref (slot-value digit 'possible-values) i)))))


(defclass sudoku-puzzle ()
  ((digits :initform (make-array '(9 9)))))

(defgeneric puzzle-digit (puzzle i j))
(defmethod puzzle-digit ((puzzle sudoku-puzzle) row column)
  (aref (slot-value puzzle 'digits) row column))

(defgeneric set-puzzle-digit (puzzle i j value))
(defmethod set-puzzle-digit ((puzzle sudoku-puzzle) i j value)
  "don't use!"
  (setf (aref (slot-value puzzle 'digits) i j) value))

(defmethod initialize-instance :after ((puzzle sudoku-puzzle) &key)
  (loop for row from 0 to 8 do
       (loop for column from 0 to 8 do
	    (set-puzzle-digit puzzle row column (make-instance 'sudoku-digit)))))

(defgeneric remove-possible-value-for-row (puzzle row value))
(defmethod remove-possible-value-for-row ((puzzle sudoku-puzzle) row value)
  (loop for column from 0 to 8 do (remove-possible-value (puzzle-digit puzzle row column) value)))

(defgeneric remove-possible-value-for-column (puzzle column value))
(defmethod remove-possible-value-for-column ((puzzle sudoku-puzzle) column value)
  (loop for row from 0 to 8 do (remove-possible-value (puzzle-digit puzzle row column) value)))

(defgeneric remove-possible-value-for-block (puzzle row column value))
(defmethod remove-possible-value-for-block ((puzzle sudoku-puzzle) row column value)
  (let ((start-row (* 3 (floor row 3)))
	(start-column (* 3 (floor column 3))))
    (loop for r from 0 to 2 do
	 (loop for c from 0 to 2 do
	      (remove-possible-value (puzzle-digit puzzle (+ r start-row) (+ c start-column)) value)))))

(defgeneric set-digit-value (puzzle i j value))
(defmethod set-digit-value ((puzzle sudoku-puzzle) i j value)
  (let ((the-digit (puzzle-digit puzzle i j)))
    (when (not (possible-value-p the-digit value))
      (error (format nil "~a is not a valid value for ~a" value the-digit)))
    (setf (digit the-digit) value)
    (when (plusp value)
      (remove-possible-value-for-row puzzle i value)
      (remove-possible-value-for-column puzzle j value)
      (remove-possible-value-for-block puzzle i j value)
      (add-possible-value the-digit value))))

(defgeneric set-fixed-digit-value (puzzle i j value))
(defmethod set-fixed-digit-value ((puzzle sudoku-puzzle) i j value)
  (setf (digit (puzzle-digit puzzle i j)) value)
  (when (plusp value)
    (remove-possible-value-for-row puzzle i value)
    (remove-possible-value-for-column puzzle j value)
    (remove-possible-value-for-block puzzle i j value)
    (setf (slot-value (puzzle-digit puzzle i j) 'possible-values) (copy-seq #(0 0 0 0 0 0 0 0 0 0)))
    (add-possible-value (puzzle-digit puzzle i j) value)))

(defmethod print-object ((puzzle sudoku-puzzle) stream)
  (loop for row from 0 to 8 do
       (loop for column from 0 to 8 do
	    (let ((digit (puzzle-digit puzzle row column)))
	      (format stream "~a(~a) " (if digit (digit digit) "?") (if digit (number-possible-values digit) "?"))
	      (when (= 2 (mod column 3))
		(format stream " "))))
       (format stream "~%")
       (when (= 2 (mod row 3))
	 (format stream "~%"))))

(defun fill-sudoku-puzzle-from-data (puzzle data)
  (loop for row-data in (rest data)
     and row-idx = 0 then (1+ row-idx) do
       (loop for char across row-data
	  and col-idx = 0 then (1+ col-idx)	     
	     when (plusp (read-from-string (format nil "~a" char))) do
	    (set-fixed-digit-value puzzle row-idx col-idx (read-from-string (format nil "~a" char))))))

(defun solve-single-without-guessing (puzzle)
  (loop for row from 0 to 8 do
       (loop for column from 0 to 8 do
	    (when (and (zerop (digit (puzzle-digit puzzle row column)))
		       (= 1 (length (possible-values (puzzle-digit puzzle row column)))))
	      (set-digit-value puzzle row column (first (possible-values (puzzle-digit puzzle row column))))))))

(defun number-holes (puzzle)
  (loop for row from 0 to 8 summing
       (loop for column from 0 to 8 
	  counting (zerop (digit (puzzle-digit puzzle row column))))))

(defun solve-without-guessing (puzzle)
  (let ((holes-before 1)
	(holes-after 0))
    (loop until (= holes-before holes-after) do
	 (setf holes-before (number-holes puzzle))
	 (solve-single-without-guessing puzzle)
	 (setf holes-after (number-holes puzzle)))
    (zerop holes-after)))

(defun nth-sudoku-puzzle (n &key try-solve)
  (let ((puzzle (make-instance 'sudoku-puzzle)))
    (fill-sudoku-puzzle-from-data puzzle (nth n (read-problem-n-data 96)))
    (when try-solve (solve-without-guessing puzzle))
    (values puzzle (number-holes puzzle))))

(defun copy-puzzle (puzzle)
  (let ((new-puzzle (make-instance 'sudoku-puzzle)))
    (loop for row from 0 to 8 do
	 (loop for column from 0 to 8 do
	      (when (not (zerop (digit (puzzle-digit puzzle row column))))
		(set-digit-value new-puzzle row column (digit (puzzle-digit puzzle row column))))))
    new-puzzle))

(defun is-in-conflicting-state (puzzle)
  (let ((conflict nil))
    (loop for row from 0 to 8 do
	 (loop for column from 0 to 8 do 
	      (let ((the-digit (puzzle-digit puzzle row column)))
		(when (and (zerop (digit the-digit))
			   (null (possible-values the-digit)))
		  (setf conflict t)))))
    conflict))


(defun top-3-filled (puzzle)
  (and (not (zerop (digit (puzzle-digit puzzle 0 0))))
       (not (zerop (digit (puzzle-digit puzzle 0 1))))
       (not (zerop (digit (puzzle-digit puzzle 0 2))))))

(defmacro do-on-all-digits (puzzle row column digit &body body)
  `(loop for ,row from 0 to 8 do
	(loop for ,column from 0 to 8 do
	     (let ((,digit (puzzle-digit ,puzzle ,row ,column)))
	       ,@body))))
(defun find-digit-to-change (puzzle)
  (let ((found-digit nil)
	(found-possibilities 9)
	found-row
	found-column)
    (loop for row from 0 to 8 do
	 (loop for column from 0 to 8 do
	      (let* ((the-digit (puzzle-digit puzzle row column))
		     (the-possibilities (number-possible-values the-digit)))
		(when (and (zerop (digit the-digit))
			   (< the-possibilities found-possibilities))
		  (setf found-row row)
		  (setf found-column column)
		  (setf found-digit the-digit)
		  (setf found-possibilities the-possibilities)))))
    (list found-digit found-row found-column)))

(defun is-solved (puzzle)
  (zerop (number-holes puzzle)))

(defun solve-sudoku-puzzle (puzzle)
  (if (is-solved puzzle)
      puzzle
      (destructuring-bind (digit-to-change row column) (find-digit-to-change puzzle)
	(loop for new-digit-value in (possible-values digit-to-change) do
	     (let ((new-puzzle (copy-puzzle puzzle)))
	       (set-digit-value new-puzzle row column new-digit-value)
	       (unless (is-in-conflicting-state new-puzzle)
		 (solve-without-guessing new-puzzle))
	       (if (is-solved new-puzzle)
		   (progn
		     (return-from solve-sudoku-puzzle new-puzzle))	       
		   (progn 
		     (unless (is-in-conflicting-state new-puzzle)
		       (let ((possible-solution (solve-sudoku-puzzle new-puzzle)))
			 (if (is-solved possible-solution)
			     (return-from solve-sudoku-puzzle possible-solution))))))))
	puzzle)))
	     
  
(defun top-3-value (puzzle)
  (apply #'(lambda (x y z) (+ z (* 10 (+ y (* 10 x))))) 
	 (mapcar #'(lambda (column) (digit (puzzle-digit puzzle 0 column))) '(0 1 2))))

(defun problem-96 ()
  (let ((solved-puzzles (loop for i from 0 to 49 collect (solve-sudoku-puzzle (nth-sudoku-puzzle i :try-solve t)))))
    (reduce #'+ (mapcar #'top-3-value solved-puzzles))))
