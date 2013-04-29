(in-package :asdf)

(defsystem :dining-philosophers
    :name "A Star"
    :version "0.1.1"
    :serial t
    :depends-on (:cost-calc2)
    :components ((:module "dining-philosophers"
			  :components ((:file "main")))))
