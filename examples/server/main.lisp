(in-package :cost)

(defclass job ($node) 
  ((completed :accessor completed :initform nil)
   (priority :accessor priority :initarg :prio :initform 0)
   (code :accessor code :initform '(sleep .1))
   (hdd :accessor hdd :initarg :hdd)
   (ram :accessor ram :initarg :ram)
   (mhz :accessor mhz :initarg :mhz)))

(defclass server ($node) 
  ((channel :accessor channel :initform (gensym "SERVER"))
   (bid-channel :accessor bid-channel :initform (gensym "SERVER-BID"))
   (run-channel :accessor run-channel :initform (gensym "RUN-CHANNEL"))
   (available? :accessor available? :initform t)
   (hdd :accessor hdd :initarg :hdd)
   (ram :accessor ram :initarg :ram)
   (mhz :accessor mhz :initarg :mhz)))

(defmethod print-object ((server server) stream)
  (format stream "#<server hdd: ~a ram: ~a mhz: ~a>" (hdd server) (ram server) (mhz server)))

(defmethod print-object ((job job) stream)
  (format stream "#<job hdd: ~a ram: ~a mhz: ~a>" (hdd job) (ram job) (mhz job)))

(defclass bid ($node)
  ((job :accessor job :initarg :job)
   (server :accessor server :initarg :server)))

(defclass job-queue ($node) 
  ((%children :accessor %children :initform (list))))

(defmethod make-children ((job-queue job-queue))
#+nil  (pprint (%children job-queue) #.*standard-output*)
  (%children job-queue))

(defun random-job ()
  (make-instance 'job
		 :prio (random 10)
		 :hdd (random 10)
		 :ram (random 10)
		 :mhz (random 10)))

(defun random-server ()
  (make-instance 'server
		 :hdd (random 10)
		 :ram (random 10)
		 :mhz (random 10)))

(defmethod solution-cost ((bid bid)) 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro squared (expression)
    (let ((x (gensym "x")))
      `(let ((,x ,expression))
	 (* ,x ,x)))))

(defmethod execution-cost ((bid bid))
  (sqrt (+ (squared (- (hdd (server bid)) (hdd (job bid))))
	   (squared (- (ram (server bid)) (ram (job bid))))
	   (squared (- (mhz (server bid)) (mhz (job bid)))))))

(defmethod solution-cost ((job job))
  (priority job))

(defmethod execution-cost ((job job)) 0)
#+nil (with-generation ($OS-THREADS-ENGINE)
	($define server-send (job server servers)
	  (choice ((null server) nil)
		  ((not (null server))
		   (par
		     (send job (chan (channel server) t))
		     (server-send job (car servers) (cdr servers))))))
    
	($define server-receive (server servers)
	  (choice ((null server) nil)
		  ((not (null server))
		   (seq (receive bid-name (chan (bid-channel server) t))
			(call 
			 list* 
			 bid-name
			 (server-receive (car servers) (cdr servers)))))))

	($define bid-server (server)
	  (seq (receive job (chan (channel server) t))
	       (receive bid (make-instance 'bid :job job :server server))
	       (pprint (list 'bidding job server (cost bid)) #.*standard-output*)
	       (send bid (chan (bid-channel server) t))
	       (bid-server server)))

	($define job-server (server)
	  (seq (receive job (chan (run-channel server) t))
	       (pprint (list 'running job server) #.*standard-output*)
	       (setf (completed job) t)
	       (eval (code job))
	       (job-server server)))

	($define run-server (server)
	  (pprint (list 'running server) #.*standard-output*)
	  (par (bid-server server)
	       (job-server server)))

	($define send-queue (job-queue)
	  (seq 
	    (choice ((null (%children job-queue)))
		    ((%children job-queue)
		     (seq
		       (pprint (list 'put-queue (length (%children job-queue)) (bt:current-thread)) #.*standard-output*)
		       (send job-queue (chan arbiter)))))
	    (send-queue job-queue)))


	($define maintain-queue (job-queue)
	  (seq
	    (receive job (chan job-queue))
	    (pprint (list 'queue-job job) #.*standard-output*)
	    (push job (%children job-queue))
	    
	    (maintain-queue job-queue)))

	($define start-queue-thread (job-queue)
	  (par
	    (send-queue job-queue)
	    (maintain-queue job-queue)))

	($define run-servers (server servers)
	  (choice ((null servers)
		   (run-server server))
		  ((not (null servers))
		   (par (run-server server)
			(run-servers (car servers) (cdr servers))))))

	($define random-job-queue ()
	  (seq 
	    (receive job (random-job))
	    (pprint (list 'add-job job) #.*standard-output*)
	    (send job (chan job-queue))
	    (sleep .1)
	    (random-job-queue)))

	($define arbiter (servers)
	  (seq
	    (pprint (list 'arbiter) #.*standard-output*)
	    (receive job-queue (chan arbiter))
	    (pprint (list 'arbiter 'received job-queue))
	    (receive job (min job-queue))
	    (setf (%children job-queue) (remove job (%children job-queue)))
	    (pprint (list 'arbiter 'picked job))
	    (pprint (list 'job-thing job) #.*standard-output*)
	    (server-send job (car servers) (cdr servers))
	    (receive bids (server-receive (car servers) (cdr servers)))
	    (receive winning-bid (min bids))
	    (send job (chan (run-channel (server winning-bid)) t))
	    (arbiter servers)))

	($define run (servers)
	  (seq (receive queue (make-instance 'job-queue))
	       (par (arbiter servers)
		    (start-queue-thread queue)
		    (run-servers (car servers) (cdr servers))
		    (random-job-queue)))))
       
(with-generation ($OS-THREADS-ENGINE)
	($define server-send (job server servers)
	  (choice ((null server) nil)
		  ((not (null server))
		   (par
		     (send job (chan (channel server) t))
		     (server-send job (car servers) (cdr servers))))))
    
	($define server-receive (server servers)
	  (choice ((null server) nil)
		  ((not (null server))
		   (seq (receive bid-name (chan (bid-channel server) t))
			(call 
			 list* 
			 bid-name
			 (server-receive (car servers) (cdr servers)))))))

	($define bid-server (server)
	  (seq (receive job (chan (channel server) t))
	       (receive bid (make-instance 'bid :job job :server server))	       
	       (send bid (chan (bid-channel server) t))
	       (bid-server server)))

	($define job-server (server)
	  (seq (receive job (chan (run-channel server) t))
	       (setf (completed job) t)
	       (eval (code job))
	       (job-server server)))

	($define run-server (server)
	  (par (bid-server server)
	       (job-server server)))

	($define send-queue (job-queue)
	  (seq 
	    (choice ((null (%children job-queue)))
		    ((%children job-queue)
		     (seq
		       (send job-queue (chan arbiter)))))
	    (send-queue job-queue)))


	($define maintain-queue (job-queue)
	  (seq
	    (receive job (chan job-queue))
	    (push job (%children job-queue))
	    
	    (maintain-queue job-queue)))

	($define start-queue-thread (job-queue)
	  (par
	    (send-queue job-queue)
	    (maintain-queue job-queue)))

	($define run-servers (server servers)
	  (choice ((null servers)
		   (run-server server))
		  ((not (null servers))
		   (par (run-server server)
			(run-servers (car servers) (cdr servers))))))

	($define random-job-queue ()
	  (seq 
	    (receive job (random-job))
	    (send job (chan job-queue))
	    (sleep .1)
	    (random-job-queue)))

	($define arbiter (servers)
	  (seq
	    (receive job-queue (chan arbiter))
	    (receive job (min job-queue))
	    (setf (%children job-queue) (remove job (%children job-queue)))
	    (server-send job (car servers) (cdr servers))
	    (receive bids (server-receive (car servers) (cdr servers)))
	    (receive winning-bid (min bids))
	    (send job (chan (run-channel (server winning-bid)) t))
	    (arbiter servers)))

	($define run (servers)
	  (seq (receive queue (make-instance 'job-queue))
	       (par (arbiter servers)
		    (start-queue-thread queue)
		    (run-servers (car servers) (cdr servers))
		    (random-job-queue)))))

(defun test-servers (i)
  (let ((servers (loop repeat i collect (random-server))))
    (run servers)))
