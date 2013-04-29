(in-package :cost)

(defvar *queue*)

(defmethod children ((node A*-node))
       (let ((children (mapcar #'get-loc (connections node))))
	 (setf (marked? node) t)
	 (map nil (lambda (c) 
		    (unless (marked? c)
		      (setf (parent c) node))) children)
	 (loop for c in children do (pushnew c *queue*))
	 (setf *queue* (remove-if #'marked? *queue*))
	 *queue*))

(defmethod create-epsilon ((node A*-node))
  (make-instance 'A*-epsilon :parent node))

(defmethod create-terminal ((node A*-node))
  (make-instance 'A*-goal :parent node))

(defmethod execution-cost ((node A*-node)) 0)

(defmethod solution-cost ((node A*-node))
  (if (parent node)
      (+ (distance node (parent node)) (solution-cost (parent node)))
      0))

(defmethod execution-cost ((node A*-epsilon))
  (distance (parent node) (get-loc 'goal)))

(defmethod solution-cost ((node A*-epsilon))
  (solution-cost (parent node)))

(defmethod solution-cost ((node A*-goal))
  (+ (distance node (parent node)) (solution-cost (parent node))))
	  
