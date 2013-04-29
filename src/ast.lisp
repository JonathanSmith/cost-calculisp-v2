(in-package :cost)

(defvar *parent* nil)
(defvar *channels* (make-hash-table))

(defclass $AST () ())

(defclass $ENGINE () ())

(defclass $NON-TERMINAL ($AST)
  ((parent :accessor parent :initform *parent*)
   (children :accessor children :initarg :children :initform nil)))

(defclass $TERMINAL ($AST) 
  ((parent :accessor parent :initform *parent*)))

(defclass $ROOT ($AST) 
  ((children :accessor children :initarg :children :initform nil)))

(defclass $FN ($NON-TERMINAL)
  ((name :accessor fn-name :initarg :fn-name :initform nil)
   (args :accessor args :initarg :args)))

(defclass $MAP ($TERMINAL)
  ((fn :accessor fn :initarg :fn)
   (args :accessor args :initarg :args)))

(defclass $PAR ($NON-TERMINAL) ())
(defclass $SEQ ($NON-TERMINAL) ())
(defclass $MIN ($NON-TERMINAL) ())
(defclass $MAX ($NON-TERMINAL) ())

(defclass $CHOICE ($NON-TERMINAL) 
  ((guards :initarg :guards :accessor guards)))

(defclass $CALL ($TERMINAL) 
  ((fn :accessor fn :initarg :fn)
   (args :accessor args :initarg :args)))

(defclass $ATOMIC ($TERMINAL) 
  ((code :accessor code :initarg :code :initform nil)))

(defclass $CHAN ($TERMINAL) 
  ((name :accessor chan-name :initarg :name)
   (channel :accessor channel :initarg :channel)))

(defclass $DCHAN ($CHAN) ())
				  
(defclass $SEND ($NON-TERMINAL) 
  ((channel :accessor channel :initarg :channel :initform nil)))

(defclass $RECEIVE ($TERMINAL) 
  ((variable :accessor variable :initarg :variable)
   (channel :accessor channel :initarg :channel :initform nil)))

(defparameter *$fns* (make-hash-table))

(defparameter *$specials* '(par seq min max choice atomic send receive $lambda define chan call map))

(defun $specialp (token)
  (member token *$specials*))
 
(defun $fnp (token)
  (gethash token *$fns*))


#+nil (define foo (bar baz)
	(seq (receive x (+ 2 3 4))
	     (receive y (chan b))
	     (receive z (+ x y))
	     (send z (chan 'b))))
#+nil (define frob (bar baz)
	(seq 
	 (par (recieve x (+ 2 3 4))
	      (recieve y (+ 4 5 6)))
	 (recieve z (+ x y))))

#+nil (define frub (bar baz)
	(choice
	 ((= bar 1) (+ bar baz))
	 ((= baz 1) (- bar baz))
	 ((= (+ bar baz) 2) (/ bar baz))))
#+nil 
($define par-test ()
  (par (seq
	 (pprint "start channel a" #.*standard-output*)
	 (send 'Q (chan a))
	 (pprint "finish channel a" #.*standard-output*))
       (seq
	 (pprint "start channel b" #.*standard-output*)
	 (receive P (chan a))
	 (format #.*standard-output* "~&finish channel b received: ~a" P))))
