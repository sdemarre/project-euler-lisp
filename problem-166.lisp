(defmacro range-list (start end)
  `',(loop for i from start to end collect i))
(defmacro get-table-row (table row size)
  (let ((column (gensym)))
  `(mapcar #'(lambda (,column) (aref ,table ,row ,column)) (range-list 0 ,(1- size)))))
(defmacro get-table-column (table column size)
  (let ((row (gensym)))
  `(mapcar #'(lambda (,row) (aref ,table ,column ,row)) (range-list 0 ,(1- size)))))
(defmacro get-table-diagonal1 (table size)
  (let ((row (gensym)))
    `(mapcar #'(lambda (,row) (aref ,table ,row ,row)) (range-list 0 ,(1- size)))))
(defmacro get-table-diagonal2 (table size)
  (let ((row (gensym)))
    `(mapcar #'(lambda (,row) (aref ,table ,row (- ,(1- size) ,row))) (range-list 0 ,(1- size)))))

(defun row-sum (table row)
  (reduce #'+ (get-table-row table row 4)))
(defun column-sum (table column)
  (reduce #'+ (get-table-column table column 4)))
(defun diagonal1-sum (table)
  (reduce #'+ (get-table-diagonal1 table 4)))
(defun diagonal2-sum (table)
  (reduce #'+ (get-table-diagonal2 table 4)))


(defun problem-166 ()
  (let ((table (make-array '(4 4) :initial-element 0)))
    (loop for row from 0 to 3 collect row)))






