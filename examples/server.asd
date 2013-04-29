(in-package :asdf)

(defsystem :server
  :name "Server Load Balancing Algorithm"
  :version "0.1.1"
  :serial t
  :depends-on (:cost-calc2)
  :components ((:module "server"
			:components ((:file "main")))))
