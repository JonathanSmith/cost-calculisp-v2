(in-package :cost)

(defun %gain (node next-node)
  (let* ((dataset (subset node))
	 (total (length dataset))
	 (values (node-attribute-values next-node))
	 (next-attribute (attribute next-node))
	 (partition nil)
	 (countlist nil))

    (when (= total 0) (return-from %gain 0))

    (dolist (value values)
      (let ((sublist nil)
	    (count 0))
	(mapcar #'(lambda (instance) 
		    (when (eql (funcall next-attribute instance) value)
		      (incf count)
		      (push instance sublist))) dataset)
	(push count countlist)
	(push sublist partition)))

    (- (entropy node)
       (reduce #'+ (mapcar (lambda (subset count)
			     (/ (* count (entropy node subset)) total))
			   partition countlist)))))

(defmethod solution-cost ((node id3-root))
  (entropy node))

(defmethod execution-cost ((node id3-root)) 0)

(defmethod solution-cost ((node id3-attribute))
  (gain node (parent node)))

(defmethod execution-cost ((node id3-attribute)) 0)

(defun proportions (values dataset set-size attribute &aux proportion-list)
  (dolist (value values)
    (let ((count 0))
      (mapcar #'(lambda (instance) 
		  (when (eql (funcall attribute instance) value)
		    (incf count)))
	      dataset)
      (push (if (= set-size 0) 0 (/ count set-size)) proportion-list)))
  proportion-list)

(with-generation ($ENGINE)
  ($define entropy (node &optional (dataset (subset node)))
	   (seq
	    (receive attribute (attribute node))
	    (receive values (node-attribute-values node))
	    (receive set-size (length dataset))
	    (receive proportion-list (proportions values dataset set-size attribute)) 
	    (proportion-entropy (car proportion-list) (cdr proportion-list))))

  ($define proportion-entropy (c proportions)
	   (choice ((numberp c)
		    (call + (choice ((> c 0)
				     (* (- c) (log c 2)))
				    ((not (> c 0))
				     0))
			  (proportion-entropy
			   (car proportions)
			   (cdr proportions))))
		   ((not (numberp c)) 0)))

  ($define gain (node &optional (parent (parent node)))
	   (choice ((typep parent 'id3-attribute)		    
		    (%gain parent node))
		   ((typep parent 'id3-value)
		    (gain node (parent parent)))
		   ((typep parent 'id3-root)
		    (entropy node))))

  ($define id3 (node)
	   (choice ((typep node 'id3-root)
		    (seq (receive attribute ($max node))
			 (call list
			       (attribute attribute)
			       (id3 attribute))))
		   ((typep node 'id3-attribute)
		    (map id3 node))
		   ((typep node 'id3-value)
		    (seq (receive attribute ($max node))
			 (choice ((typep attribute 'id3-attribute)
				  (call list
					(value node)
					(attribute attribute)
					(id3 attribute)))
				 ((not (typep attribute 'id3-attribute))
				  (call list
					(value node)
					(or (most-common-result node #'outcome) '?)))))))))
	       

(defun most-common-result (node function)
  (or (remove-duplicates (mapcar function (subset node)))
      (most-common-result (parent node) function)))
