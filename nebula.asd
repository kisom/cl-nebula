;;;; nebula.asd

(asdf:defsystem #:nebula
  :serial t
  :description "Describe nebula here"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT"
  :depends-on (#:cl-ppcre
	       #:flexi-streams
	       #:ironclad
	       #:local-time
	       #:postmodern
	       ;;#:restas
	       #:st-json
	       #:uuid)
  :components ((:file "package")
	       (:file "util")
	       (:file "blob")
	       (:file "entry")
	       (:file "db")
               (:file "nebula")))

