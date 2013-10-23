(in-package :project-euler)

(defmacro is-char-ok (pos char)
  `(char= (elt string ,pos) ,char))

(defun is-206-compliant (string)
  (and (is-char-ok 0 #\1)
       (is-char-ok 2 #\2)
       (is-char-ok 4 #\3)
       (is-char-ok 6 #\4)
       (is-char-ok 8 #\5)
       (is-char-ok 10 #\6)
       (is-char-ok 12 #\7)
       (is-char-ok 14 #\8)
       (is-char-ok 16 #\9)
       (is-char-ok 18 #\0)))

(defun problem-206 ()
  (loop for i from 100000000 to 141421356 do
       (let* ((tmp (* i 10))
	      (tmpsqr (* tmp tmp))
	      (tmps (format nil "~d" tmpsqr)))
	 (declare (type fixnum tmp tmpsqr))
	 (when (is-206-compliant tmps)
	   (return tmp)))))
	   
			  
