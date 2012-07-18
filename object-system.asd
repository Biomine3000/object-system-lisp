(asdf:defsystem #:object-system
    :serial t
    :depends-on (#:usocket
		 #:cl-json
		 #:flexi-streams
		 #:cl-ppcre
		 #:ironclad
		 #:babel
		 #:lift
		 #:trivial-gray-streams)
    :components ((:file "object-system")
		 (:file "tests")))
