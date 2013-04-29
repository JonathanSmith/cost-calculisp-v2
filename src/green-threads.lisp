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
    ,(%generate (first (children $SEND)) engine)))

(defmethod %generate (($PAR $PAR) (engine $GREEN-THREADS-ENGINE))
  (let ((thread (gensym "THREAD"))
	(threads (gensym "THREADS"))
	(results (gensym "RESULTS"))
	(children (gensym "CHILDREN"))
	(child (gensym "CHILD")))
    `(let ((,threads (list)))
       (gt:with-green-thread
	 ,@(mapcar #'(lambda (x)
		       (if (typep x '$MAP)
			   `(loop for ,child in (children ,(%generate (first (args x)) engine)) do
				 (push 
				  (gt:get-join-future
				   (gt:with-green-thread				       
				     (,(fn x) ,child))) ,threads))
			   `(push 
			     (gt:get-join-future 
			      (gt:with-green-thread
				,(%generate x engine))) ,threads)))
		   (children $PAR)))
       (mapcar #'gt:future-values ,threads))))

(defmethod %generate (($FN $FN) (engine $GREEN-THREADS-ENGINE))
  (if (fn-name $FN)
      `(defun ,(fn-name $FN) (,@(args $FN))
	 (cl-cont:with-call/cc
	 ,@(mapcar #'(lambda (x) (%generate x engine)) (children $FN))))
      `(lambda (,@(args $FN))
	 (cl-cont:with-call/cc
	 ,@(mapcar #'(lambda (x) (%generate x engine)) (children $FN))))))
