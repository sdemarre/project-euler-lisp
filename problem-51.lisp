(in-package :project-euler)

(defun replace-all-digits (number digit-to-replace new-digit-value)
  (digits-to-number (loop for digit in (number-to-digits number)
		       collect (if (= digit digit-to-replace) new-digit-value digit))))

(defun all-replacements-of (number digit-to-replace)
  (loop for new-digit-value from 0 to 9 
     collect (replace-all-digits number digit-to-replace new-digit-value)))

(defun prime-replacements-of (number digit-to-replace)
  (remove-if-not #'cllib:primep (all-replacements-of number digit-to-replace)))

(defun problem-51 ()
  (let (stop resulting-number)
    (loop for current-number = 111858 then (1+ current-number)
       when (cllib:primep current-number) do
	 (loop for digit-to-replace in (remove-duplicates (number-to-digits current-number))
	    when (= 8 (length (prime-replacements-of current-number digit-to-replace))) do 
	      (setf stop t)
	      (setf resulting-number current-number)
	      (return))
	 (when stop
	   (return)))
    resulting-number))