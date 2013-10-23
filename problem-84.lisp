(in-package :project-euler)

(defstruct board-state 
  name
  next-state-fun
  (number-of-visits 0))

(defstruct board
  states)

(defun dice-roll (dice-type)
  "random number from 1 to dice-type"
  (1+ (random dice-type)))

(defun dice-sum (number-dice dice-type)
  "sum of number-dice throws of dice-type dice, and a boolean indicating if all dice had the same value"
  (let ((rolls (loop for dice from 1 to number-dice collect (dice-roll dice-type))))
    (values (reduce #'+ rolls) (= (length (remove-duplicates rolls)) 1))))

(defun board-states (board)
  "returns a list of the boards states"
  (declare (ignorable board)))

(defun board-number-states (board)
  ""
  (length (board-states board)))

(defun board-state-at-position (board position)
  "returns the state at position"
  (elt (board-states board) position))

(defun board-state-position (board state)
  "returns the position of the state"
  (position state (board-states board)))

(defun board-initial-state (board)
  (board-state-at-position board 0))

(defun state-name (state)
  (board-state-name state))

(defun board-state-by-name (board name)
  (let ((result (find-if #'(lambda (state) (string= name (state-name state))) (board-states board))))
    (if result
	result
	(error (format nil "No state with name ~a" name)))))

(defun board-jail-state (board)
  (board-state-by-name board "jail"))

(defun get-next-state (current-state board)
  "returns the state 1 step forward"
  (let* ((current-position (board-state-position board current-state))
	 (next-position (if (= (1+ current-position) (board-number-states board)) 
			    0 
			    (1+ current-position))))	
    (board-state-at-position board next-position)))

(defun get-previous-state (current-state board)
  "returns the state 1 step backward"
  (let* ((current-position (board-state-position board current-state))
	 (next-position (if (= (1- current-position) -1) 
			    (1- (board-number-states board))
			    (1- current-position))))
    (board-state-at-position board next-position)))
  
(defun default-board-change-state-function (current-state board number-dice dice-type)
  "returns the new state, and a boolean indicating if all dice had the same value"
  (let* ((state current-state)
	 (dice-results (multiple-value-list (dice-sum number-dice dice-type))))
    (dotimes (i (first dice-results))
      (setf state (get-next-state state board)))
    (values state (second dice-results))))

(defun community-chest-change-state (current-state board)
  "returns the next state for a given cc state"
  (let ((roll (dice-roll 16)))
    (cond ((= roll 1) (board-initial-state board))
	  ((= roll 2) (board-jail-state board))
	  (t current-state))))
  

(defun chance-change-state (current-state board)
  "returns the next state for a give chance state"
  (let ((roll (dice-roll 16)))
    ;(format t "[roll ~a]" roll)
    (cond ((= roll 1) (board-initial-state board))
	  ((= roll 2) (board-jail-state board))
	  ((= roll 3) (board-state-by-name board "c1"))
	  ((= roll 4) (board-state-by-name board "e3"))
	  ((= roll 5) (board-state-by-name board "h2"))
	  ((= roll 6) (board-state-by-name board "r1"))
	  ((or (= roll 7) (= roll 8)) (next-railway-state current-state board))
	  ((= roll 9) (next-utility-state current-state board))
	  ((= roll 10) (back-3-states current-state board))
	  (t current-state))))

(defun back-3-states (current-state board)
  (flet ((f (state) (get-previous-state state board)))
    (funcall (compose #'f #'f #'f) current-state)))

(defun is-railway-state (state board)
  (declare (ignorable board))
  (member (state-name state) '("r1" "r2" "r3" "r4") :test #'string=))

(defun is-utility-state (state board)
  (declare (ignorable board))
  (member (state-name state) '("u1" "u2") :test #'string=))

(defun next-railway-state (current-state board)
  "returns the next railway state"
  (find-next-state-if #'is-railway-state current-state board))

(defun next-utility-state (current-state board)
  "returns the next utility state"
  (find-next-state-if #'is-utility-state current-state board))

(defun find-next-state-if (predicate current-state board)
  "find the next state that for which predicate holds"
  (let ((new-state current-state))
    (until (funcall predicate new-state board)
      (setf new-state (get-next-state new-state board)))
    new-state))

(defun go-to-jail-change-state (current-state board)
  (declare (ignorable current-state board))
  (board-jail-state board))

(defun create-board-from-states (states)
  (make-board :states
	      (loop for state in states collect
		   (typecase state
		     (string (make-board-state :name state :next-state-fun nil))
		     (cons (make-board-state :name (first state) :next-state-fun (symbol-function (second state))))
		     (t (error "Don't know how to handle state '~a' to create board state" state))))))

(defun create-default-board ()
  (create-board-from-states '("go"
			      "a1"
			      ("cc1" community-chest-change-state)
			      "a2"
			      "t1"
			      "r1"
			      "b1"
			      ("ch1" chance-change-state)
			      "b2"
			      "b3"
			      "jail"
			      "c1"
			      "u1"
			      "c2"
			      "c3"
			      "r2"
			      "d1"
			      ("cc2" community-chest-change-state)
			      "d2"
			      "d3"
			      "fp"
			      "e1"
			      ("ch2" chance-change-state)
			      "e2"
			      "e3"
			      "r3"
			      "f1"
			      "f2"
			      "u2"
			      "f3"
			      ("g2j" go-to-jail-change-state)
			      "g1"
			      "g2"
			      ("cc3" community-chest-change-state)
			      "g3"
			      "r4"
			      ("ch3" chance-change-state)
			      "h1"
			      "t2"
			      "h2")))

(defun remember-last-3 (new-roll previous-3-rolls)
  (list new-roll (first previous-3-rolls) (second previous-3-rolls)))

(defun problem-84 (&optional (number-dice 2) (dice-type 4))
  (let* ((board (create-default-board))
	 (current-state (first (board-states board)))
	 (current-rolls '(nil nil nil)))
    (assert (> (length (board-states board)) 0))
    (loop for i from 1 to 1000000 do 
	 (multiple-value-bind (new-state all-dice-equal)
	     (default-board-change-state-function current-state board number-dice dice-type)
	   (setf current-rolls (remember-last-3 all-dice-equal current-rolls))
	   (if (every #'identity current-rolls)
	       (progn 
		 (setf new-state (board-jail-state board)))
	       (awhen (board-state-next-state-fun new-state)
		 (setf new-state (funcall it new-state board))))
	   (incf (board-state-number-of-visits new-state))
	   (setf current-state new-state)))	   
    (let ((result (mapcar #'(lambda (state) (list (board-state-number-of-visits state)
						  (board-state-name state)
						  (board-state-position board state)))
			  (board-states board))))
      (firstn 4 (sort result #'> :key #'first)))))
	   