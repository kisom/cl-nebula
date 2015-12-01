;;;; nebula.asd

(asdf:defsystem #:nebula
  :serial t
  :description "Capability-based file store"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT"
  :version 0.2.0
  :depends-on (#:cl-ppcre
	       #:cl-who
	       #:flexi-streams
	       #:ironclad
	       #:local-time
	       #:log4cl
	       #:postmodern
	       #:swank
	       #:restas
	       #:st-json
	       #:uiop
	       #:uuid)
  :components ((:file "package")
	       (:file "util")
	       (:file "blob")
	       (:file "entry")
	       (:file "db")
               (:file "nebula")))

