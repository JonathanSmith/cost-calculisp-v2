(in-package :asdf)

(defsystem :id3
    :name "ID3 Algorithm"
    :version "0.1.1"
    :serial t
    :depends-on (:cost-calc2)
    :components ((:module "id3"
			  :serial t
			  :components ((:file "dataset")
				       (:file "nodes")
				       (:file "main")
				       (:file "schema")
				       (:file "tests")))))
