(in-package :project-euler)

(defun f-117-all (l cache)
  (+ (f-117 l 2 cache) (f-117 l 3 cache) (f-117 l 4 cache)))
(defun f-117 (l n cache)
  (cond ((aref cache l n) (aref cache l n))
	((zerop n) 1)
        ((< l n) (setf (aref cache l n) 0))
        ((= l n) (setf (aref cache l n) 1))
        (t (setf (aref cache l n) (+ (+ L 1 (- 0 n)) (loop for p from 0 to (- l n) summing (+ (f-117-all (- l (+ p n)) cache))))))))

(defun problem-117 (l)
  (let ((cache (make-array `(,(1+ l) ,(1+ l)) :initial-element nil)))
    (1+ (f-117-all l cache))))
