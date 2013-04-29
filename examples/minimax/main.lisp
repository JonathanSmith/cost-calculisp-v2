(in-package :cost)

(defvar win-conditions '((1 2 3) (4 5 6) (7 8 9)
			 (1 4 7) (2 5 8) (3 6 9)
			 (1 5 9) (3 5 7)))
(defvar possible-moves '(1 2 3 4 5 6 7 8 9))

(defclass tic-tac-toe-terminal ($terminal) ())

(defclass tic-tac-toe ($node)
  ((player :accessor player :initarg :player :initform 0)
   (possible-moves :accessor possible-moves 
		   :initarg :possible-moves
		   :initform possible-moves)
   (move :accessor move 
	 :initarg :move
	 :initform nil)))

(defmethod print-object ((node tic-tac-toe) stream)
  (let ((moves (cons node (ancestors node)))
	(board (make-array 9 :initial-element nil)))
    (dolist (node moves)
      (when (move node)
	(setf (aref board (- (move node) 1)) (if (= (player node) 1) #\x #\o))))
    
    (labels ((tile (num)
	       (or (aref board num) (format nil "~a"(1+ num)))))
      (format stream "~&|~a|~a|~a|~%" (tile 0) (tile 1) (tile 2))
      (format stream "|~a|~a|~a|~%" (tile 3) (tile 4) (tile 5))
      (format stream "|~a|~a|~a|~%" (tile 6) (tile 7) (tile 8)))))

(defmethod make-children ((node tic-tac-toe))
  (if (possible-moves node)
      (mapcar #'(lambda (move) 
		  (make-instance 
		   'tic-tac-toe
		   :move move
		   :player (logxor (player node) 1)
		   :possible-moves (remove move (possible-moves node))
		   :parent node))
	      (possible-moves node))
      (list (make-instance 'tic-tac-toe-terminal :parent node))))


(defmethod solution-cost ((node tic-tac-toe))
  (let* ((moves (cons node (ancestors node)))
	 (x-moves (mapcar #'move (remove 1 moves :key #'player)))
	 (o-moves (mapcar #'move (remove 0 moves :key #'player))))
    (cl:map nil (lambda (x) 
	       (when (null (set-difference x x-moves))
		 (return-from solution-cost most-negative-fixnum))) win-conditions)
    (cl:map nil (lambda (x) 
	       (when (null (set-difference x o-moves))
		 (return-from solution-cost most-positive-fixnum))) win-conditions)
    0))

(defmethod solution-cost ((node tic-tac-toe-terminal))
  (solution-cost (parent node)))

(defmethod execution-cost ((node tic-tac-toe-terminal))
  0)

(defmethod execution-cost ((node tic-tac-toe))
  0)

(with-generation ($OS-THREADS-ENGINE)
  ($define maximin (node)
    (choice ((typep node 'tic-tac-toe)
	     (max (par (map minimax node))))
	    ((typep node '$terminal)
	     node)))
  ($define minimax (node)
    (choice ((typep node 'tic-tac-toe)
	     (min (par (map maximin node))))
	    ((typep node '$terminal)
	     node))))

(defun play ()
  (labels ((w/l/t? (node)
	     (let* ((state (cost node)))
		 (cond ((eql state most-negative-fixnum)
			(format t "~a~& o wins!" node)
			(return-from play nil))
		       ((eql state most-positive-fixnum)
			(format t "~a~& x wins!" node)
			(return-from play nil))
		       ((eql state 0)
			(unless (and node (not (typep (first (children node)) '$terminal)))
			  (format t "draw!")
			  (return-from play nil))))))
	     
	   (player-sel (node)
	     (let* ((moves (possible-moves node)))		   
	       (format t "~a~%Make A Move~%Available-moves: ~a~%" node moves)
	       (sleep .1)
	       (let ((selection (read)))
		 (dolist (node (children node))
		   (when (eql (move node) selection)
		     (return-from player-sel node)))
		 (format t "~&invalid selection ~a" selection)
		 (player-sel node))))

	   (computer-sel (node)
	     (let ((selection (minimax node)))
	       (first (intersection (children node)
				    (ancestors selection)))))
	   (play-turn (node)
	     (setf node (player-sel node))
	     (w/l/t? node)
	     (setf node (computer-sel node))
	     (w/l/t? node)
	     (play-turn node)))
    (play-turn (make-instance 'tic-tac-toe))))
