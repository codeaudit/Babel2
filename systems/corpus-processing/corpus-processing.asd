(in-package :asdf)

(defsystem :corpus-processing
  :depends-on (:utils :cl-fad :cl-json :trivial-timeout)
  :components 
  ((:file "corpus-processing")
   (:file "json-corpus-processing")
   (:file "json-stream-processing")
   ))
	

