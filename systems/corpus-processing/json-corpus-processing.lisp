;;                                                              
;; JSON Corpus Processing                                       
;;                                                              
;; System developed for parallel processing of large JSON files.
;; Uses the 'jq' command line tool and cl-json for encoding and
;; decoding.
;; 
;; The corpus processing already takes care of encoding/decoding.
;; The 'function' will receive decoded JSON data (i.e. an alist)
;; and should send back an alist that will than be encoded to
;; string before written to output.
;;
;; Regarding the input:
;; By default, we assume the JSON file has an array at the top
;; level that contains JSON objects.
;; Alternatively, if a symbol is passed to 'object-key', the
;; JSON file has an object at the top level and an array as
;; value for that key.
;; Example: if the object-key is 'data', we read .data in the
;; JSON file
;; Finally, if 'object-key' is a list, the array we will iterate
;; over is situated multiple levels deep in the JSON file.
;; Example: if the object-key is '(foo bar), we read .foo.bar
;; in the JSON file.
;;
;; Regarding the output:
;; The output will always consist of a JSON array on the top level
;; with JSON objects inside. These objects are the ones returned
;; by 'function'.
;;
;; It is possible to pass keyword arguments to 'function', by
;; providing them with 'function-kwargs'. Pass them as a plist
;; e.g. '(:key-1 value-1 :key-2 value-2)
;;                                                            
;; Jens - June 2018                                             
;;                                                              

;; Load with:
;; (ql:quickload :corpus-processing)

(in-package :utils)

(export '(json-process-corpus))

(defun empty-directory-p (dirpath)
  "Check if the given directory is empty"
  (null (cl-fad::list-directory dirpath)))

(defun delete-all-files-in-folder (dirpath)
  "Remove all files in the given directory"
  (let ((all-files (cl-fad::list-directory dirpath)))
    (mapcar #'delete-file all-files)))

(defun all-nonempty-json-files-in-folder (dirpath)
  "Return all files in the given directory that are
   non-empty JSON files. The non-emptiness is checked
   using the 'jq' tool"
  (let ((all-files (cl-fad::list-directory dirpath)))
    (remove-if-not #'(lambda (pname)
                       (and (string= (pathname-type pname) "json")
                            (> (jq-number-of-objects pname) 0)))
                   all-files)))

(defun jq-installed-p ()
  "Checks if the jq tool is installed"
  (exec-and-return "which" "jq"))

(defun quote-string (string)
  "Adds extra quotes in to a string"
  (format nil "\"~a\"" string))

(defun unquote-string (string)
  "Removes extra quotes from a string"
  (subseq string 1 (1- (length string))))

(defun jq-json-array-p (inputfile &key key)
  "Checks if the json data is an array"
  (let ((jq-args  (cond ((null key) "type")
                        ((symbolp key) (format nil ".~a | type" key))
                        ((listp key) (format nil "~{.~a~} | type" key)))))
    (string= (unquote-string (first (exec-and-return "jq" jq-args (namestring inputfile))))
             "array")))
    
(defun jq-number-of-objects (inputfile &key key)
  "Returns the number of objects in the json file.
   If a key is specified, use the value of that key."
  (let* ((jq-args (cond ((null key) "length")
                        ((symbolp key) (format nil ".~a | length" key))
                        ((listp key) (format nil "~{.~a~} | length" key))))
         (value (exec-and-return "jq" jq-args (namestring inputfile))))
    (parse-integer (first value))))

#+:LISPWORKS7+
(defun all-threads-dead (thread-list)
  "Returns t if all processes in thread-list are of status dead."
  (let ((all-dead t)
        (statuses (mapcar #'mp:process-whostate thread-list)))
    (dolist (status statuses)
      (unless (equalp "dead" status)
        (setf all-dead nil)))
    all-dead))

#+:CCL
(defun all-threads-dead (thread-list)
  "Returns t if all processes in thread-list are of status dead."
  (let ((all-dead t)
        (statuses (mapcar #'ccl:process-whostate thread-list)))
    (dolist (status statuses)
      (unless (equalp "reset" status)
        (setf all-dead nil)))
    all-dead))

(defun read-json-entries (inputfile key start end)
  "Reads the json entries between index start and end start the
   inputfile"
  (let* ((jq-args (cond ((null key) (format nil ".[~a:~a]" start end))
                        ((symbolp key) (format nil ".~a[~a:~a]" key start end))
                        ((listp key) (format nil "~{.~a~}[~a:~a]" key start end))))
         (value (exec-and-return "jq" jq-args (namestring inputfile))))
    (cl-json:decode-json-from-string
     (list-of-strings->string value :separator ""))))

(defun write-tmp-file (list-of-alists batch-start batch-end thread-nr dir)
  "Writes the result of a thread to a tmp file"
  (let* ((filename (format nil "json-batch-~a-~a-thread-~a" batch-start batch-end thread-nr))
         (outpath (merge-pathnames dir (make-pathname :name filename :type "json"))))
    (with-open-file (stream outpath :direction :output)
      (write-string (cl-json:encode-json-to-string list-of-alists) stream))))

#+:LISPWORKS7+
(defun process-list-of-json-objects (function function-kwargs list-of-alists mailbox timeout)
  "Applies function to list-of-json-objects and returns the processed list-of-objects"
  (mailbox-send mailbox
                (cons (car list-of-alists)
                      (mapcar #'(lambda (alist)
                                  (if function-kwargs
                                    (apply function alist timeout function-kwargs)
                                    (funcall function alist timeout)))
                              (cdr list-of-alists)))))

#+:CCL
(defun process-list-of-json-objects (function function-kwargs list-of-alists timeout)
  "Applies function to list-of-json-objects and returns the processed list-of-objects"
  (cons (car list-of-alists)
        (mapcar #'(lambda (alist)
                    (if function-kwargs
                      (apply function alist timeout function-kwargs)
                      (funcall function alist timeout)))
                (cdr list-of-alists))))

(defun json-process-corpus (&key function
                                 function-kwargs
                                 object-key
                                 inputfile
                                 outputfile
                                 (tmpdir (babel-pathname :directory '(".tmp" "json-process-corpus-tmp")))
                                 (remove-tmp-data t)
                                 (timeout nil)
                                 (number-of-threads 4)
                                 (number-of-objects-per-thread 2000))
  "Applies 'function' with 'function-kwargs' to every JSON object in the inputfile and
   writes the result in outputfile. 
   A higher number-of-threads results in a higher speed (if these threads are available).
   A higher number-of-entries-per-thread results in a higher speed, but also in a higher
   memory use."
  ;; Detect if the jq command line tool is installed
  (unless (jq-installed-p)
    (error (format nil "Could not detect the 'jq' command line tool.~%Install it with homebrew:~%~% > brew install jq~%")))

  ;; Validate the input
  (assert (string= (pathname-type inputfile) "json"))
  (assert (string= (pathname-type outputfile) "json"))
  (assert (jq-json-array-p inputfile :key object-key))

  ;; Printing some information
  (format t "~%~%****************** Started Corpus Processing ******************")
  (format t "~%~%Inputfile: ~a" inputfile)
  (format t "~%Outputfile: ~a" outputfile)
  (format t "~%Applying function: ~a" function)
  (when function-kwargs (format t "~%Passing function arguments: ~a" function-kwargs))
  (when object-key (format t "~%Reading JSON key(s): ~a" object-key))
  (format t "~%Storing temporary data in: ~a" tmpdir)
  (format t "~%~%Threads: ~a" number-of-threads)
  (format t "~%Objects per thread: ~a" number-of-objects-per-thread)
  (format t "~%Objects per batch: ~a" (* number-of-threads number-of-objects-per-thread))
  (when timeout (format t "~%Timeout per thread: ~as" timeout))

  ;; Count number of objects in file
  (format t "~%Total number of objects: ")
  (let ((start-time (get-universal-time))
        (number-of-objects (jq-number-of-objects inputfile :key object-key))
        (number-of-objects-per-batch (* number-of-objects-per-thread number-of-threads))
        success)
    (format t "~a" number-of-objects)

    ;; Clear the outputfile
    (with-open-file (stream outputfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (declare (ignore stream)))
    
    ;; Check if the tmpdir exist and if it is empty
    (ensure-directories-exist tmpdir)
    (unless (empty-directory-p tmpdir)
      (delete-all-files-in-folder tmpdir))

    ;; Divide inputfile in batches, that are processed sequentially (avoids reading whole corpus in memory)
    (multiple-value-bind (number-of-complete-batches number-of-objects-in-last-batch)
        (floor number-of-objects number-of-objects-per-batch)
      (format t "~%Number of batches: ~a complete + ~a objects in extra batch.~%" number-of-complete-batches number-of-objects-in-last-batch)
      (let ((start 0)
            (end number-of-objects-per-batch))
        (do ((n 0 (1+ n)))
            ((= n number-of-complete-batches) (format t "~%(Finished)"))
          (format t "~%Started complete batch ~a/~a..." (+ 1 n) number-of-complete-batches)
          (setf success
                (append (process-batch-multi-threaded function function-kwargs inputfile object-key
                                                      number-of-threads number-of-objects-per-batch
                                                      start end tmpdir timeout)
                        success))
          (incf start number-of-objects-per-batch)
          (incf end number-of-objects-per-batch))
        (when (> number-of-objects-in-last-batch 0)
          (format t "~%Started extra batch...")
          (setf success
                (append (process-batch-multi-threaded function function-kwargs inputfile object-key
                                                      number-of-threads number-of-objects-in-last-batch
                                                      start (+ start number-of-objects-in-last-batch) tmpdir timeout)
                        success))
          (format t "~%(Finished)"))))

    ;; Merge the JSON files written by every batch into outputfile
    (format t "~%Merging temporary data files into ~a" outputfile)
    (let ((all-tmp-files (all-nonempty-json-files-in-folder tmpdir)))
      (exec-and-return "jq" "-s" (format nil "add ~{~a ~}" (mapcar #'namestring all-tmp-files))
                       ">" (namestring outputfile)))

    ;; When specified, remove the tmp data
    (when remove-tmp-data
      (format t "~%Removing temporary data files")
      (delete-all-files-in-folder tmpdir))

    ;; Finish
    (let ((finish-time (get-universal-time)))
      (multiple-value-bind (h m s)
          (seconds-to-hours-minutes-seconds (- finish-time start-time))
        (format t "~%~%Processing took ~a hours, ~a minutes and ~a seconds." h m s)))
    (format t "~%~%***************** Finished Corpus Processing *****************")
    (setf success (substitute 0 nil success))
    (format t "~%~%******************** Overall Accuracy: ~,3f ********************~%~%"
            (average success))
    (average success)))

#+:LISPWORKS7+
(defun process-batch-multi-threaded (function
                                     function-kwargs
                                     inputfile
                                     object-key
                                     number-of-threads
                                     number-of-objects
                                     start
                                     end
                                     tmpdir
                                     timeout)
  "takes a batch of objects, divides them over threads,
   applies function on each object and writes
   the output to a file in tmpdir"
  (let ((list-of-thread-batches nil))
        ;; Divide objects over threads
        (multiple-value-bind (entries-per-thread entries-last-thread)
            (floor number-of-objects number-of-threads)
          ;; Read from input
          (format t "~%      Launching ~a threads with ~a entries and 1 thread with ~a entries "
                  (- number-of-threads 1) entries-per-thread (+ entries-per-thread entries-last-thread))
          (let ((thread-start start)
                (thread-end (+ start entries-per-thread)))
            (dotimes (n (- number-of-threads 1)) ;; For each thread, except last
              (push (cons (+ 1 n)
                          (read-json-entries inputfile object-key
                                             thread-start thread-end))
                    list-of-thread-batches)
              (incf thread-start entries-per-thread)
              (incf thread-end entries-per-thread))
            ;; For last thread
            (push (cons number-of-threads
                        (read-json-entries inputfile object-key
                                           thread-start (+ thread-start entries-per-thread entries-last-thread)))
                  list-of-thread-batches)))
       
        ;; process each thread-batch
        (let* ((mailbox (make-mailbox :name "batch-mailbox"))
               (thread-list (mapcar #'(lambda (thread-batch)
                                        (process-run-function "json-processing" '()
                                                              #'process-list-of-json-objects
                                                              function function-kwargs thread-batch mailbox timeout))
                                    list-of-thread-batches))
               (mailbox-messages nil))
          ;; Wait for messages
          (mp:process-wait "waiting for threads to finish..." 'all-threads-dead thread-list)
          ;; Read batches from mailbox
          (while (not (mp:mailbox-empty-p mailbox))
            (push (mp:mailbox-read mailbox) mailbox-messages))
          ;; Write batches to tmpdir
          (loop for message in (sort mailbox-messages #'< :key #'car)
                for thread-nr from 0
                for message-content = (cdr message)
                for success = (mapcar #'car message-content)
                for output-data = (mapcar #'cdr message-content)
                do (write-tmp-file output-data start end thread-nr tmpdir)
                append success))))

#+CCL
(defun process-batch-multi-threaded (function
                                     function-kwargs
                                     inputfile
                                     object-key
                                     number-of-threads
                                     number-of-objects
                                     start
                                     end
                                     tmpdir
                                     timeout)
  "takes a batch of objects, divides them over threads,
   applies function on each object and writes
   the output to a file in tmpdir"
  (let ((list-of-thread-batches nil))
        ;; Divide objects over threads
        (multiple-value-bind (entries-per-thread entries-last-thread)
            (floor number-of-objects number-of-threads)
          ;; Read from input
          (format t "~%      Launching ~a threads with ~a entries and 1 thread with ~a entries "
                  (- number-of-threads 1) entries-per-thread (+ entries-per-thread entries-last-thread))
          (let ((thread-start start)
                (thread-end (+ start entries-per-thread)))
            (dotimes (n (- number-of-threads 1)) ;; For each thread, except last
              (push (cons (+ 1 n)
                          (read-json-entries inputfile object-key
                                             thread-start thread-end))
                    list-of-thread-batches)
              (incf thread-start entries-per-thread)
              (incf thread-end entries-per-thread))
            ;; For last thread
            (push (cons number-of-threads
                        (read-json-entries inputfile object-key
                                           thread-start (+ thread-start entries-per-thread entries-last-thread)))
                  list-of-thread-batches)))
       
        ;; process each thread-batch
        (let ((thread-list (loop repeat number-of-threads
                                 collect (ccl:make-process "json-processing")))
              (mailbox-messages nil))
          ;; preset the threads
          (loop for thread in thread-list
                for thread-batch in list-of-thread-batches
                do (ccl:process-preset thread #'process-list-of-json-objects
                                       function function-kwargs thread-batch timeout))
          ;; enable the process
          (loop for thread in thread-list
                do (ccl:process-enable thread))
          ;; Wait for threads to finish
          (ccl:process-wait "waiting for threads to finish..." 'all-threads-dead thread-list)
          ;; Join the threads
          (loop for thread in thread-list
                do (push (ccl:join-process thread) mailbox-messages))
          ;; Filter out empty messages
          (setf mailbox-messages
                (remove nil mailbox-messages))
          ;; Write batches to tmpdir
          (loop for message in (sort mailbox-messages #'< :key #'car)
                for thread-nr from 0
                for message-content = (cdr message)
                for success = (mapcar #'car message-content)
                for output-data = (mapcar #'cdr message-content)
                do (write-tmp-file output-data start end thread-nr tmpdir)
                append success))))