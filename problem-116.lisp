(in-package :project-euler)

(defun f-116 (l n cache)
  (cond ((aref cache l n) (aref cache l n))
        ((< l n) (setf (aref cache l n) 0))
        ((= l n) (setf (aref cache l n) 1))
        (t (setf (aref cache l n) (+ (+ L 1 (- 0 n)) (loop for p from 0 to (- l n) summing (f-116 (- l (+ p n)) n cache)))))))

(defun problem-116 (l)
  (let ((cache (make-array `(,(1+ l) ,(1+ l)) :initial-element nil)))
    (+ (f-116 l 2 cache) (f-116 l 3 cache) (f-116 l 4 cache))))





