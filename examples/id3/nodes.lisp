(in-package :cost)

(defclass id3-node ($node)
  ((dataset-symbol :accessor dataset-symbol :initarg :dataset-symbol)))

(defclass id3-attribute (id3-node)
  ((attribute :accessor attribute :initarg :attribute)))

(defclass id3-value (id3-node)
  ((value :accessor value :initarg :value)
   (%subset :accessor %subset :initarg :subset)))

(defclass id3-root (id3-node) ())

(defmethod find-subset ((node id3-value))
  (let ((parent-subset (subset (parent node)))
	(parent-attribute (attribute (parent node)))
	(node-value (value node)))
    (remove-if-not (lambda (instance) (eql (funcall parent-attribute instance) node-value)) parent-subset)))

(defmethod subset ((node id3-root))
  (dataset (dataset-symbol node)))

(defmethod subset ((node id3-attribute))
  (subset (parent node)))

(defmethod subset ((node id3-value))
  (if (%subset node)
      (%subset node)
      (let ((subset (find-subset node)))
	(setf (%subset node) subset)
	subset)))

(defun node-attribute-values (node)
  (attribute-values (attribute node) (dataset-symbol node)))
 
(defmethod all-attributes ((node id3-node))
  (all-attributes (dataset-symbol node)))

(defmethod used-attributes ((node id3-node))
  (mapcar #'attribute (remove-if-not (lambda (object) (typep object 'id3-attribute)) (ancestors node))))

(defmethod remaining-attributes ((node id3-node))
  (set-difference (all-attributes node) (used-attributes node)))

(defmethod make-children ((node id3-root))
  (let ((attributes (all-attributes node)) 
	children)
    (dolist (attr attributes)
      (push (make-instance 'id3-attribute
			   :parent node
			   :dataset-symbol
			   (dataset-symbol node)
			   :attribute attr) children))
    children))

(defmethod make-children ((node id3-value))
  (let ((attributes (remaining-attributes node))
	children)
    (dolist (attr attributes)
      (push (make-instance 'id3-attribute
			   :parent node
			   :dataset-symbol
			   (dataset-symbol node)
			   :attribute attr) children))
    children))

(defmethod make-children ((node id3-attribute))
  (let ((values (node-attribute-values node))
	children)
    (dolist (value values)
      (push (make-instance 'id3-value
			   :parent node
			   :subset nil
			   :dataset-symbol
			   (dataset-symbol node)
			   :value value) children))
    children))
