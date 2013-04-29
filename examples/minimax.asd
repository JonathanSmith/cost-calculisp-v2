(in-package :asdf)

(defsystem :minimax
    :name "Minimax Algorithm"
    :version "0.1.1"
    :serial t
    :depends-on (:cost-calc2)
    :components ((:module "minimax"
			  :components ((:file "main")))))
