(in-package :cost)

(defclass $GREEN-THREADS-ENGINE ($ENGINE) ())

(defmethod %generate (($CHAN $CHAN) (engine $GREEN-THREADS-ENGINE))
  (setf (channel $CHAN) (make-instance 'gt:channel))
  (case *channel-context*
    (:receive
     `(gt:recv/cc (channel ,$CHAN) :blockp t))
    (:send $CHAN)))

(defmethod %generate (($DCHAN $DCHAN) (engine $GREEN-THREADS-ENGINE))
  (let ((channel (gensym "CHANNEL"))
	(chan-name (gensym "CHANNEL-NAME")))
    `(let* ((,chan-name ,(%generate (chan-name $DCHAN) engine))
	    (,channel (gethash ,chan-name *channels* (make-instance '$CHAN
								   :name (gensym)
								   :channel (make-instance 'gt:channel)))))
       (setf (gethash ,chan-name *channels*) ,channel)
       ,(case *channel-context*
	      (:receive
	       `(gt:recv/cc (channel ,channel) :blockp t))
	      (:send channel)))))

(defmethod %generate (($SEND $SEND) (engine $GREEN-THREADS-ENGINE))
  `(gt:send/cc
    (channel ,(%generate (channel $SEND) engine))
    ,(%generate (first (children $SEND)) engine) :blockp t))

(defmethod %generate (($PAR $PAR) (engine $GREEN-THREADS-ENGINE))
  (let ((thread (gensym "THREAD"))
	(threads (gensym "THREADS"))
	(results (gensym "RESULTS"))
	(children (gensym "CHILDREN"))
	(child (gensym "CHILD")))
    (let ((non-maps (remove-if (lambda (x) (typep x '$MAP)) (children $PAR)))
	  (maps (remove-if-not (lambda (x) (typep x '$MAP)) (children $PAR))))
      `(gt:with-green-thread
	 (let ((,threads (list ,@(mapcar (lambda (x) 
					   `(gt:with-green-thread
					      ,(%generate x engine))) non-maps))))
	   ,@(mapcar #'(lambda (x)		       
			 `(loop for ,child in (children ,(%generate (first (args x)) engine)) do
			       (push 				  
				(gt:with-green-thread				       
				  (,(fn x) ,child)) ,threads))) maps)
	   (loop for ,thread in ,threads collect
		(gt:join-thread ,thread)))))))

(defmethod %generate (($FN $FN) (engine $GREEN-THREADS-ENGINE))
  (if (fn-name $FN)
      `(cl-cont:defun/cc ,(fn-name $FN) (,@(args $FN))	 
	 ,@(mapcar #'(lambda (x) (%generate x engine)) (children $FN)))
      `(lambda (,@(args $FN))
	 (cl-cont:with-call/cc
	 ,@(mapcar #'(lambda (x) (%generate x engine)) (children $FN))))))
