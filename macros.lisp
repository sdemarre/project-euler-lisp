(in-package :project-euler)

(defun expand-conditions-list (l n cache condition-value-list)
  (loop for (condition value) in condition-value-list collect (list condition `(setf (aref ,cache ,l ,n) ,value))))

(defmacro cond-with-cache (l n cache &rest condition-value-list)
  `(cond ((aref ,cache ,l ,n) (aref ,cache ,l ,n))
	 ,@(expand-conditions-list l n cache condition-value-list)))

(defmacro cond-without-cache (l n cache &rest condition-value-list)
  (declare (ignorable l n cache))
  `(cond ,@condition-value-list))
