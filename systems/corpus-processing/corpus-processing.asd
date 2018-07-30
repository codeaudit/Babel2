(in-package :asdf)

(defsystem :corpus-processing
  :depends-on (:utils :cl-fad :cl-json :trivial-timeout)
  :components 
  (#+:lw(:file "corpus-processing")
   (:file "json-corpus-processing")))
	

