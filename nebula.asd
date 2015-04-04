;;;; nebula.asd

(asdf:defsystem #:nebula
  :serial t
  :description "Describe nebula here"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT"
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
	       #:uuid)
  :components ((:file "package")
	       (:file "util")
	       (:file "blob")
	       (:file "entry")
	       (:file "db")
               (:file "nebula")
	       (:file "www")))

