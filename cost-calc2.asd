(in-package :asdf)

(defsystem :cost-calc2
  :name "cost"
  :version "0.1.1"
  :serial t
  :depends-on (:chanl :bordeaux-threads :green-threads)
  :components 
  ((:module "src"
	    :serial t
	    :components ((:file "defpackage")
			 (:file "ast")
			 (:file "nodes")
			 (:file "backend")
			 (:file "green-threads")
			 (:file "os-threads")))))
