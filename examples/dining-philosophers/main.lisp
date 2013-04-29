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
