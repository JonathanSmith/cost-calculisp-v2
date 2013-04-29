(in-package :cost)

(defun wrap-form (form)
  (if (listp form)
      (cond (($specialp (car form)) form)
	    (($fnp (car form)) `(call ,@form))
	    (t `(atomic ,form)))
      `(atomic ,form)))

(defun wrap-body (body)
  (mapcar #'wrap-form body))


(defmacro $lambda (args &body body)
  `(let ((*parent* (make-instance '$FN :args ',args)))
     (setf (children *parent*) (list ,@(wrap-body body)))
     *parent*))

(defmacro $define (name args &body body)
  `(let ((*parent* (make-instance '$FN :args ',args :fn-name ',name)))
     (setf (gethash ',name *$fns*) *parent*)
     (setf (children *parent*) (list ,@(wrap-body body)))
     *parent*))


(defmacro par (&body body)
  `(let ((*parent* (make-instance '$PAR)))
     (setf (children *parent*) (list ,@(wrap-body body)))
     *parent*))


(defmacro seq (&body body)
  `(let ((*parent* (make-instance '$SEQ)))
     (setf (children *parent*) (list ,@(wrap-body body)))
     *parent*))


(defmacro min (&body body)
  `(let ((*parent* (make-instance '$MIN)))
     (setf (children *parent*) (list ,@(wrap-body body)))
     *parent*))


(defmacro max (&body body)
  `(let ((*parent* (make-instance '$MAX)))
     (setf (children *parent*) (list ,@(wrap-body body)))
     *parent*))


(defmacro choice (&body body)
  `(let ((*parent* (make-instance '$CHOICE)))
     (setf (guards *parent*) (list ,@(wrap-body (mapcar #'first body))))
     (setf (children *parent*) (list ,@(wrap-body (mapcar #'second body))))
     *parent*))


(defvar *enclosing* :choice)
(defvar *channel-context* :send)
;;min,max,choice
(defvar *cost-combination-fn* #'+)

(defmethod cost (($AST $AST))
  `(funcall *cost-combination-fn* ,(solution-cost $AST) ,(execution-cost $AST)))

(defmethod solution-cost (($PAR $PAR))
  (let ((costs (mapcar #'solution-cost (children $PAR))))
    (case *enclosing*
      (:min `(reduce #'cl:min (list ,@costs)))
      (:max `(reduce #'cl:max (list ,@costs)))
      (:choice `(elt (list ,@costs) (random ,(length costs)))))))

(defmethod solution-cost (($SEQ $SEQ))
  (let ((costs (mapcar #'solution-cost (children $SEQ))))
    (case *enclosing*
      (:min `(reduce #'cl:min (list ,@costs)))
      (:max `(reduce #'cl:max (list ,@costs)))
      (:choice `(elt (list ,@costs) (random ,(length costs)))))))

(defmethod solution-cost (($MAX $MAX))
  `(reduce #'cl:max (list ,@(mapcar #'solution-cost (children $MAX)))))

(defmethod solution-cost (($MIN $MIN))
  `(reduce #'cl:min (list ,@(mapcar #'solution-cost (children $MIN)))))

(defmethod solution-cost (($CHOICE $CHOICE))
  `(elt (list ,@(mapcar #'solution-cost (children $CHOICE))) (random ,(length (children $CHOICE)))))

(defmethod solution-cost (($ATOMIC $ATOMIC))
  `(let ((*enclosing* ,*enclosing*))
     (solution-cost ,(code $ATOMIC))))

(defmethod execution-cost (($PAR $PAR))
  (let ((costs (mapcar #'execution-cost (children $PAR))))
    (case *enclosing*
      (:min `(reduce #'cl:min (list ,@costs)))
      (:max `(reduce #'cl:max (list ,@costs)))
      (:choice `(elt (list ,@costs) (random ,(length costs)))))))

(defmethod execution-cost (($SEQ $SEQ))
  (let ((costs (mapcar #'execution-cost (children $SEQ))))
    (case *enclosing*
      (:min `(reduce #'cl:min (list ,@costs)))
      (:max `(reduce #'cl:max (list ,@costs)))
      (:choice `(elt (list ,@costs) (random ,(length costs)))))))

(defmethod execution-cost (($MAX $MAX))
  `(reduce #'cl:max (list ,@(mapcar #'execution-cost (children $MAX)))))

(defmethod execution-cost (($MIN $MIN))
  `(reduce #'cl:min (list ,@(mapcar #'execution-cost (children $MIN)))))

(defmethod execution-cost (($CHOICE $CHOICE))
 `(elt (list ,@(mapcar #'execution-cost (children $CHOICE))) (random ,(length (children $CHOICE)))))

(defmethod execution-cost (($ATOMIC $ATOMIC))
  `(let ((*enclosing* ,*enclosing*))
     (execution-cost ,(code $ATOMIC))))

(defmacro call (fn &rest args)
  `(make-instance '$CALL :fn ',fn :args (list ,@(wrap-body args))))

(defmacro map (fn &rest args)
  `(make-instance '$MAP :fn ',fn :args (list ,@(wrap-body args))))


(defmacro chan (&optional name dynamic)
  (let ((channel (gensym "CHANNEL")))
    (if dynamic
	`(let ((,channel (make-instance '$DCHAN :name ,(wrap-form name))))
	   ,channel)
	`(if (gethash ',name *channels*)
	     (gethash ',name *channels*)
	     (let ((,channel (make-instance '$CHAN :name ',name)))
	       (setf (gethash ',name *channels*) ,channel)
	       ,channel)))))

(defmacro send (expression channel)
  `(let ((*parent* (make-instance '$SEND)))
     (setf (channel *parent*) ,channel)
     (setf (children *parent*)	  
	   (list ,(wrap-form expression)))	
     *parent*))

(defmacro receive (variable channel)
  `(let ((*parent* 
	  (make-instance '$RECEIVE :variable ',variable)))
     (setf (channel *parent*)
	   ,(if (listp channel)
		(wrap-form channel)
		channel))
     *parent*))

(defun blocked ()
  (throw :blocked :blocked))

(defmacro atomic (body)
  `(make-instance '$ATOMIC :code ',body))

(defmethod %generate (($FN $FN) (engine $ENGINE))
  (if (fn-name $FN)
      `(defun ,(fn-name $FN) (,@(args $FN))
	 ,@(mapcar #'(lambda (x) (%generate x engine)) (children $FN)))
      `(lambda (,@(args $FN))
	 ,@(mapcar #'(lambda (x) (%generate x engine)) (children $FN)))))

(defmethod %generate (($CALL $CALL) (engine $ENGINE))
  `(,(fn $CALL) ,@(mapcar (lambda (x) (%generate x engine)) (args $CALL))))

(defmethod %generate (($MAP $MAP) (engine $ENGINE))
  `(mapcar ',(fn $MAP) (concatenate 'list ,@(mapcar (lambda (x) `(children ,(%generate x engine))) (args $MAP)))))

(defmethod %generate (($SEQ $SEQ) (engine $ENGINE))
  (labels ((generate-1 (child rest)
	     (when child
	       (case (type-of child)
		 ($RECEIVE
		  `((let ,(%generate child engine)
		      ,@(generate-1 (car rest) (cdr rest)))))
		 (otherwise
		  `(,(%generate child engine)
		     ,@(generate-1 (car rest) (cdr rest))))))))
    (let ((result (generate-1 (car (children $SEQ)) (cdr (children $SEQ)))))
      (if (eql (first result) 'let)
	  result
	  `(progn ,@result)))))

(defmethod %generate (($RECEIVE $RECEIVE) (engine $ENGINE))
  (let ((*channel-context* :receive))
    `((,(variable $RECEIVE) ,(%generate (channel $RECEIVE) engine)))))

(defmethod %generate (($ATOMIC $ATOMIC) (engine $ENGINE))
  (code $ATOMIC))

(defmethod $min ((node $node))
  (let ((children (children node)))
    ($min children)))

(defmethod $min ((list list))
  (if (> (length list) 1)
      (let* ((min (list (first list)))
	     (min$ (cost (first min))))
	(dolist (next (rest list))
	  (let ((next$ (cost next)))
	    (cond ((< next$ min$)
		   (setf min$ next$)
		   (setf min (list next)))
		  ((= next$ min$)
		   (push next min)))))
	(nth (random (length min)) min))
      (first list)))

(defmethod %generate (($MIN $MIN) (engine $ENGINE))
  (if (= (length (children $MIN)) 1)
      `($min ,(%generate (first (children $MIN)) engine))
      (let ((statement 
	     (let ((i 0))
	       (mapcar #'(lambda (x) `(list ,(cost x) ,(prog1 i (incf i)))) (children $MIN))))
	    (idx (gensym "IDX")))

	`(let ((,idx (second (reduce #'(lambda (x y)
					 (if (< (first x) (first y)) x y))
				     (list ,@statement) :key #'first))))
	   (case ,idx
	     ,@(let ((i 0))
		    (mapcar #'(lambda (x) 
				`(,(prog1 i (incf i)) ,(%generate x engine)))
			    (children $MIN))))))))

(defmethod $max ((node $node))
  (let ((children (children node)))
    ($max children)))

(defmethod $max ((list list))
  (if (> (length list) 1)
      (let* ((max (list (first list)))
	     (max$ (cost (first max))))
	(dolist (next (rest list))
	  (let ((next$ (cost next)))
	    (cond ((> next$ max$)
		   (setf max$ next$)
		   (setf max (list next)))
		  ((= next$ max$)
		   (push next max)))))
	(nth (random (length max)) max))
      (first list)))

			    
(defmethod %generate (($MAX $MAX) (engine $ENGINE))
  (if (= (length (children $MAX)) 1)
      `($max ,(%generate (first (children $MAX)) engine))
      (let ((statement 
	     (let ((i 0))
	       (mapcar #'(lambda (x) `(list ,(cost x) ,(prog1 i (incf i))))
		       (children $MAX))))
	    (idx (gensym "IDX")))
	`(let ((,idx (second (reduce #'(lambda (x y)
					 (if (> (first x) (first y)) x y))
				     (list ,@statement) :key #'first))))
	   (case ,idx
	     ,@(let ((i 0))
		    (mapcar #'(lambda (x) 
				`(,(prog1 i (incf i)) ,(%generate x engine)))
			    (children $MAX))))))))

(defmethod %generate (($CHOICE $CHOICE) (engine $ENGINE))
  (let ((statement
	 (let ((i 0))
	   (mapcar #'(lambda (x) `(list ,(%generate x engine) ,(prog1 i (incf i))))
		   (guards $CHOICE))))
	(idxes (gensym "IDXES"))
	(idx (gensym "IDX")))
    `(let* ((,idxes (mapcar #'second (remove-if-not #'first (list ,@statement))))
	    (,idx (nth (random (length ,idxes)) ,idxes)))
       (ecase ,idx
	 ,@(let ((i 0))
		(mapcar #'(lambda (x) `(,(prog1 i (incf i)) ,(%generate x engine)))
			(children $CHOICE)))))))

(defmacro with-generation ((engine-class) &rest forms)
  (let ((engine (gensym "engine")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((*channels* (make-hash-table :synchronized t)))
	 (let ((,engine (make-instance ',engine-class)))
	   ,@(loop for form in forms collect
		  `(compile (eval (%generate ,form ,engine)))))))))
