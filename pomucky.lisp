;pomucky pro 06,7,9.lisp
(defun make-point (x y) (set-y (set-x (make-instance 'point) x) y))

(defmethod set-center ((self circle) value)
(setf (slot-value self 'center) value)
  self)