(in-package :asdf)

(defsystem :a-star
    :name "A Star"
    :version "0.1.1"
    :serial t
    :depends-on (:cost-calc2 :graph-utils)
    :components ((:module "a-star"
			  :components ((:file "define-graph")
				       (:file "nodes")
				       (:file "main")))))
    
