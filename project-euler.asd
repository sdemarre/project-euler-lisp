;;; -*- Lisp -*-
(defun euler-problem (number)
  (list :file (format nil "problem-~a" number) :depends-on '("package" "project-euler" "basic-math" "macros")))

(defmacro define-project-euler-system (&rest problems)
  `(progn
     (asdf:defsystem #:project-euler
    :depends-on (#:cl-graph #:metatilities #:split-sequence #:anaphora #:iterate)
    :components ((:file "package")
		 (:file "macros")
		 (:file "project-euler" :depends-on ("package"))
		 (:file "basic-math" :depends-on ("package"))
		 ,@(mapcar #'euler-problem problems)))
     (defparameter *project-euler-problems* ',problems)))


; missing (120 79 18)
; 18 is available in problem-67.lisp
(define-project-euler-system 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 19 20 21 22 23 24 25 26 27 28 29 30
			     31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56
			     57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 80 81 82 83 85
			     87 89 91 92 96 97 99 101 102 104 108 112 114 115 116 117 123 124 125 145 155
			     173 179 187 188 205 206)
 
