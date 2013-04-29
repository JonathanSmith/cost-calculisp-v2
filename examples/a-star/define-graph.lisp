(in-package :cost)

(defparameter *all-nodes* (make-hash-table))

(defstruct point 
  (x)
  (y))

(defgeneric x (point))
(defgeneric y (point))
(defmethod x ((point point))
  (point-x point))
(defmethod y ((point point))
  (point-y point))

(defclass A*-node ($node)
  ((name :accessor node-name :initarg :name)
   (marked? :accessor marked? :initform nil)
   (point :accessor point :initarg :point)
   (connections :accessor connections :initarg :connections)))

(defclass A*-goal (A*-node $terminal)
  ())

(defclass A*-epsilon (A*-node $epsilon)
  ())

(defmethod x ((point A*-node))
  (x (point point)))
(defmethod y ((point A*-node))
  (y (point point)))

(defmacro location (name x y &rest connections)
  (let ((location (gensym "point")))
    `(let ((,location (make-instance 'A*-node
				       :name ',name
				       :point (make-point :x ,x :y ,y)
				       :connections ',connections)))
	 (setf (gethash ',name *all-nodes*) ,location))))

(defmacro goal (name x y &rest connections)
  (let ((goal (gensym "goal")))
    `(let ((,goal (make-instance 'A*-goal
				   :name ',name
				   :point (make-point :x ,x :y ,y)
				   :connections ',connections)))
	 (setf (gethash ',name *all-nodes*) ,goal)
	 (setf (gethash 'goal *all-nodes*) ,goal))))

(defun get-loc (name)
  (gethash name *all-nodes*))

(defmacro square (num)
  (let ((gnum (gensym "num")))
    `(let ((,gnum ,num))
       (* ,gnum ,gnum))))

(defun point-distance (np gp)
  (sqrt (+ (square (- (x np) (x gp))) (square (- (y np) (y gp))))))

(defmethod distance ((node A*-node) (goal A*-node))
  (point-distance node goal))
