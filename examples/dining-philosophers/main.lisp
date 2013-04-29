(in-package :cost)

(defmacro set-table (philosopher-count &key (engine '$OS-THREADS-ENGINE))
  (let ((philosophers (loop for i from 0 below philosopher-count collect
			   (list (intern (format nil "PHILOSOPHER-~a" i))
				 (intern (format nil "PHILOSOPHER-~a-LEFT" i))
				 (intern (format nil "PHILOSOPHER-~a-RIGHT"i))
				 (intern (format nil "PHILOSOPHER-~a-RIGHT" (mod (1- i) philosopher-count)))
				 (intern (format nil "PHILOSOPHER-~a-LEFT"(mod (1+ i) philosopher-count)))))))
    `(with-generation (,engine)       
       ,@(mapcar (lambda (philosopher-spec)
		   (destructuring-bind (philosopher left-hand right-hand philosopher-left philosopher-right) philosopher-spec
		     `($define ,philosopher (left-fork right-fork)
			(choice ((and left-fork right-fork)
				 (seq
				   (pprint (list 'eating... ',philosopher) ,*standard-output*)				   
				   (par (seq (receive temp (chan ,right-hand))
					     (send right-fork (chan ,philosopher-right)))
					(seq (receive temp (chan ,left-hand))
					     (send left-fork (chan ,philosopher-left))))
				   (receive right-fork nil)
				   (receive left-fork nil)
				   (,philosopher left-fork right-fork))) 
				((not left-fork)
				 (seq
				   (send left-fork (chan ,philosopher-left))
				   (receive left-fork (chan ,left-hand))
				   (,philosopher left-fork right-fork)))					   
				((not right-fork)
				 (seq
				   (send right-fork (chan ,philosopher-right))
				   (receive right-fork (chan ,right-hand))
				   (,philosopher left-fork right-fork))))))) philosophers)
       ($define dine ()
	 (par 
	   (,(first (car philosophers)) t t)
	   ,@(mapcar #'(lambda (x) `(,(first x) nil t)) (butlast (cdr philosophers)))
	   (,(first (first (last philosophers))) nil nil))))))


(defclass philosopher ()
  ((left-fork :accessor left-fork)
   (right-fork :accessor right-fork)
   (left-philosopher :accessor left-philosopher :initarg left-philosopher)
   (right-philosopher :accessor right-philosopher :initarg right-philosopher)
   (left-hand :accessor left-hand :initform (gensym "left-hand")) 
   (right-hand :accessor right-hand :initform (gensym "right-hand"))
   (name :accessor philosopher-name :initarg :name)))


(with-generation ($OS-THREADS-ENGINE)
  ($define philosophize (philosopher)
    (seq 
      (receive philosopher-left (left-philosopher philosopher))
      (receive philosopher-right (right-philosopher philosopher))
      (receive left-fork (left-fork philosopher))
      (receive right-fork (right-fork philosopher))
      (choice
	((and left-fork right-fork)
	 (seq
	   (pprint (list 'eating (philosopher-name philosopher)) *standard-output*)
	   (par (seq 
		  (receive tap (chan (right-hand philosopher) t))
		  (send right-fork (chan (left-hand philosopher-right) t))
		  (setf (right-fork philosopher) nil))
		(seq		
		  (receive tap (chan (left-hand philosopher) t))
		  (send left-fork (chan (right-hand philosopher-left) t))
		  (setf (left-fork philosopher) nil)))))
	((not left-fork)
	 (seq
	   (send left-fork (chan (right-hand philosopher-left) t))
	   (receive left-fork (chan (left-hand philosopher) t))
	   (setf (left-fork philosopher) left-fork)))
	((not right-fork)
	 (seq
	   (send right-fork (chan (left-hand philosopher-right) t))
	   (receive right-fork (chan (right-hand philosopher) t))
	   (setf (right-fork philosopher) right-fork))))
      (philosophize philosopher)))
  ($define dining-philosophers (philosopher philosophers)
    (par
      (choice ((null philosophers))
	      ((not (null philosophers))
	       (seq
		 (dining-philosophers (car philosophers) (cdr philosophers)))))
      (philosophize philosopher)
      )))



(defun run-dining-philosophers (&rest names)
  (let ((philosophers (loop for name in names collect
			   (make-instance 'philosopher :name name))))
    (let ((prev (first (last philosophers))))
      (cl:map nil (lambda (phil) 
		 (setf (left-philosopher phil) prev)
		 (setf prev phil))
	   philosophers))
    (let ((prev (first philosophers)))
      (cl:map nil 
	   (lambda (phil)
	     (setf (right-philosopher phil) prev)
	     (setf prev phil))
	   (reverse philosophers)))

    (setf (left-fork (first philosophers)) t)
    (setf (right-fork (first philosophers)) t)
    (setf (left-fork (first (last philosophers))) nil)
    (setf (right-fork (first (last philosophers))) nil)
    (cl:map nil (lambda (phil) 
		  (setf (left-fork phil) nil)
		  (setf (right-fork phil) t))
	 (butlast (cdr philosophers)))
    #+nil(cl:map nil #'describe philosophers)
    (dining-philosophers (car philosophers) (cdr philosophers))))
