(in-package :cost)


(progn
  (house Suburban Detached High No Nothing)
  (house Suburban Detached High Yes Nothing)
  (house Rural 	Detached 	High 	No 	Responded)
  (house Urban 	Semi-detached 	High 	No 	Responded)
  (house Urban 	Semi-detached 	Low 	No 	Responded)
  (house Urban 	Semi-detached 	Low 	Yes 	Nothing)
  (house Rural 	Semi-detached 	Low 	Yes 	Responded)
  (house Suburban 	Terrace 	High 	No 	Nothing)
  (house Suburban 	Semi-detached 	Low 	No 	Responded)
  (house Urban 	Terrace 	Low 	No 	Responded)
  (house Suburban 	Terrace 	Low 	Yes 	Responded)
  (house Rural 	Terrace 	High 	Yes 	Responded)
  (house Rural 	Detached 	Low 	No 	Responded)
  (house Urban 	Terrace 	High 	Yes 	Nothing))

(defparameter *instances* (list (make-instance 'house :outcome nil 
					 :previous 'Yes
					 :income 'High
					 :house-type 'Semi-detached
					 :district 'Rural)))
(defun run-id3-house () 
  (let ((decision-tree (id3 (make-instance 'id3-root :dataset-symbol 'house))))
    (pprint decision-tree)
    (loop for instance in *instances* collect
	 (run-decision-tree decision-tree instance))))

(defun run-decision-tree (decision-tree instance)
  (labels ((run-decision-tree-1 (tree instance)
	     (let ((selector (car tree)))	       
	       (if (listp selector)
		   (first selector)
		   (let ((next (cdr (assoc (slot-value instance selector) (second tree)))))
		     (run-decision-tree-1 next instance))))))
    (run-decision-tree-1 decision-tree instance)))
