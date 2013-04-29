(in-package :cost)

(defclass $OS-THREADS-ENGINE ($ENGINE) ())

(defmethod %generate (($CHAN $CHAN) (engine $OS-THREADS-ENGINE))
  (setf (channel $CHAN) (make-instance 'chanl::channel))
  (case *channel-context*
    (:receive
     `(chanl::recv (channel ,$CHAN) :blockp t))
    (:send $CHAN)))

(defmethod %generate (($DCHAN $DCHAN) (engine $OS-THREADS-ENGINE))
  (let ((channel (gensym "CHANNEL"))
	(chan-name (gensym "CHANNEL-NAME")))
    `(let* ((,chan-name ,(%generate (chan-name $DCHAN) engine))
	    (,channel (gethash ,chan-name *channels* 
			       (make-instance '$CHAN
					      :channel (make-instance 'chanl::channel)
					      :name (gensym)))))
       (setf (gethash ,chan-name *channels*) ,channel)

     ,(case *channel-context*
	    (:receive
	     `(chanl::recv (channel ,channel) :blockp t))
	   (:send channel)))))

(defmethod %generate (($SEND $SEND) (engine $OS-THREADS-ENGINE))
  `(chanl::send
    (channel ,(%generate (channel $SEND) engine))
    ,(%generate (first (children $SEND)) engine) :blockp t))

(defmethod %generate (($PAR $PAR) (engine $OS-THREADS-ENGINE))
  (let ((thread (gensym "THREAD"))
	(threads (gensym "THREADS"))
	(children (gensym "CHILDREN"))
	(child (gensym "CHILD")))
    `(let ((,threads (list)))
       ,@(mapcar #'(lambda (x)
		     (if (typep x '$MAP)
			 `(loop for ,child in (children ,(%generate (first (args x)) engine)) do
			       (push 
				(bt:make-thread
				 (lambda ()
				   (catch :blocked (,(fn x) ,child)))) ,threads))

			 `(push
			   (bt:make-thread 
			    (lambda () 
			      (catch :blocked ,(%generate x engine)))) ,threads)))
		 (children $PAR))
       (values-list (loop for ,thread in ,threads collect
			 (bt:join-thread ,thread))))))
