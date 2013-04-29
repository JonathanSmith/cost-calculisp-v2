(in-package :cost)

(defclass instance () ())

(defgeneric dataset (thing))
(defgeneric attribute-values (symbol dataset))
(defgeneric all-attributes (thing))

(defmacro def-dataset (name &body attributes)
  (let ((attribute-hash-sym (gensym (format nil "~a-attributes-hash" name)))
	(attribute-instance-list-sym (gensym (format nil "~a-attributes-instance-list" name)))
	(attribute-list-sym (gensym (format nil "~a-attribute-list" name)))
	(classification-sym (gensym (format nil "~a-classifier" name)))
	(class-symbol (intern (string-upcase(symbol-name name)) *package*))
	(list-sym (gensym "list-sym")))

    `(let ((,attribute-hash-sym (make-hash-table))
	   (,attribute-instance-list-sym nil)
	   (,attribute-list-sym nil)
	   (,classification-sym nil))

       ;;don't really want to modify these anyway.
       (defmethod dataset ((,(gensym) (eql ',class-symbol))) ,attribute-instance-list-sym)
       (defmethod attribute-values ((symbol symbol) (,(gensym) (eql ',class-symbol))) 
	 (gethash symbol ,attribute-hash-sym))
       (defmethod all-attributes ((,(gensym) (eql ',class-symbol))) ,attribute-list-sym)
       (defmethod classifier ((,(gensym) (eql ',class-symbol))) ,classification-sym)
       
       (defclass ,class-symbol (instance)
	 (,@(mapcar 
	     #'(lambda (attr) 
		 (let ((attr-name (first attr)))
		   `(,attr-name
		     :accessor ,attr-name 
		     :initarg 
		     ,(intern (string-upcase (symbol-name attr-name)) "KEYWORD")
		     :initform nil)))
	     attributes)))

       (setf ,attribute-list-sym ',(mapcar #'first (butlast attributes)))
       (setf ,classification-sym ',(first (first (last attributes))))
       ,@(mapcar 
	  #'(lambda (attr)
	      (let* ((name (first attr))
		     (cases (rest attr)))
		`(setf (gethash ',name ,attribute-hash-sym) ',cases)))
	  attributes)

       (defun ,(intern (string-upcase (format nil "make-~a" (symbol-name name))) *package*) ,(mapcar #'first attributes)
	 (push (make-instance ',name ,@(mapcan #'(lambda (attr) (list (intern (string-upcase (symbol-name attr)) "KEYWORD") attr))
					       (mapcar #'first attributes))) ,attribute-instance-list-sym))
       (defmacro ,name (&rest ,list-sym)
	 (let ((constructor ',(intern (string-upcase (format nil "make-~a" (symbol-name name))) *package*)))
	   `(,constructor ,@(mapcar #'(lambda (sym) (list 'quote sym)) ,list-sym)))))))
