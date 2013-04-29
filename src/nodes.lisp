(in-package :cost)

(defclass $node ()
  ((parent :accessor parent :initarg :parent :initform nil)
   (selected-child :accessor selected-child :initform nil)
   (%children :accessor %children :initarg :children :initform nil)))

(defclass $epsilon ($node) ())
(defclass $terminal ($node) ())
(defclass $root ($node) ())

(defvar *cost-combination-fn* '+)

(defmethod cost (($node $node)) 
  (funcall *cost-combination-fn* (solution-cost $node) (execution-cost $node)))

(defmethod create-epsilon ((parent $node))
  (make-instance 'epsilon :parent parent))

(defmethod create-terminal ((parent $node))
  (make-instance 'terminal :parent parent))

(defmethod ancestors ((node $node))
  (let (ancestors)
    (do ((parent (parent node) (parent parent)))
	((not parent))
      (push parent ancestors))
    ancestors))

(defmethod children ((node $node))
  (if (%children node)
      (%children node)  
      (let ((children (make-children node)))
	(setf (%children node) children)
	children)))

(defmethod solution-cost ((node $terminal))
  (solution-cost (parent node)))

(defmethod execution-cost ((node $terminal))
  (execution-cost (parent node)))

(defmethod solution-cost ((node $epsilon))
  (solution-cost (parent node)))

(defmethod execution-cost ((node $epsilon))
  (execution-cost (parent node)))
