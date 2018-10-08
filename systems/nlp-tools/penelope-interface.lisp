
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfacing with the NLP tools provided by the Penelope web service  ;;
;; Katrien and Paul, October 2017                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :nlp-tools)

(export '(get-penelope-lemmas
          get-penelope-noun-chunks
          get-penelope-named-entities
          get-penelope-pos-tags
          run-penelope-dependency-parser
          get-penelope-dependency-analysis
          get-penelope-tokens
          get-penelope-sentence-tokens
          get-penelope-text-tokens
          get-word-similarity
          get-word-embeddings
          curl-json
          guardian-data
          glove))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intefacing with using http request and json ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This used curl, but we're using DEX now directly from lisp (for using curl, you need to encode-json-as-string-for-shell!!)
;; update: using it again with lispworks until the boringssl bug is fixed   

#+lispworks
(defun curl-json (url json)
  "Send curl request and returns the answer."
  (let ((response (exec-and-return "curl" url "-H"
                                   #+lispworks (format nil "~s" "Content-Type: application/json")
                                   #-lispworks (format nil "~a" "Content-Type: application/json")
                                   "-s"   "-d "  json ))) ;; remove single quotes for non lispworks
    (when response (cl-json:decode-json-from-string (first response)))))

#-lispworks
(defun curl-json (url json)
  "Send curl request and returns the answer."
  (let ((response (dex:post url
                            :headers '((Content-Type . "application/json"))
                            :content json)))
    (when response (cl-json:decode-json-from-string response))))

;;;;;;;;;;;;;;;;;;;;;
;; Noun chunks     ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-noun-chunker (sentence &key (model "en"))
  "Call the penelope server to get the noun chunks, dependency labels
and POS tags for a sentence."
  (unless (stringp sentence)
    (if (listp sentence)
      (setf sentence (format nil "~{~a~^ ~}" sentence))
      (error "The function <run-penelope-noun-chunker> expects a string as input")))
  (let ((server-result
          (curl-json "https://www.fcg-net.org/penelope/nlp/nchunk" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;;(run-penelope-noun-chunker "April is the fourth month of the year")

(defun get-penelope-noun-chunks (sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a noun chunk."
  (let ((penelope-chunks (run-penelope-noun-chunker sentence)))
    (loop for chunk in penelope-chunks
          collect (mapcar #'(lambda (chunk)
                              (cdr (find :text chunk :key #'first)))
                          chunk))))

;;(get-penelope-noun-chunks "April is the fourth month of the year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POS tagging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-pos-tagger (sentence &key (model "en"))
  "Call the penelope server to get the POS tags for a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-pos-tagger> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/pos" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

(defun get-penelope-pos-tags (transient-structure/sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a word with its most likely POS tag."
  (let* ((sentence (if (stringp transient-structure/sentence)
                     transient-structure/sentence
                     (if (stringp (get-data transient-structure/sentence :utterance))
                       (get-data transient-structure/sentence :utterance)
                       (list-of-strings->string (get-data transient-structure/sentence :utterance)))))
         (penelope-pos-tags (run-penelope-pos-tagger sentence)))
    (loop for entry in penelope-pos-tags
          for word = (rest (assoc :text entry ))
          for tag = (rest (assoc :tag entry ))
          collect (list word tag))))

;;(get-penelope-pos-tags "April is the fourth month of the year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Named Entity Recognition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-named-entity-recognition (sentence &key (model "en"))
  "Call the penelope server to get the named entities from a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-named-entity-recognition> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/ent" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

(defun get-penelope-named-entities (transient-structure/sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a named entity."
  (let* ((sentence (if (stringp transient-structure/sentence)
                     transient-structure/sentence
                     (if (stringp (get-data transient-structure/sentence :utterance))
                       (get-data transient-structure/sentence :utterance)
                       (list-of-strings->string (get-data transient-structure/sentence :utterance)))))
         (penelope-named-entities (run-penelope-named-entity-recognition sentence)))
    (loop for entry in penelope-named-entities
          for entity = (rest (assoc :text entry ))
          for type = (rest (assoc :ent entry ))
          collect (list entity type))))

;;(get-penelope-named-entities "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dependency parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-dependency-parser (sentence &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-dependency-parser> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/dep" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;;(run-penelope-dependency-parser "April is the fourth month of the year")

(defun get-penelope-dependency-analysis (utterance &key (model "en"))
  "Returns a dependency tree analysis."
  (rest (assoc :tree (first (run-penelope-dependency-parser utterance :model model)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tokenization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-tokenizer (sentence &key (model "en"))
  "Call the penelope server to tokenize a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-tokenizer> expects a string as input"))
  (let* ((server-result
          (curl-json "https://www.fcg-net.org/penelope/nlp/tok" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;;(run-penelope-tokenizer "Paul kicked the ball. Mary caught it.")

(defun get-penelope-tokens (sentence &key (model "en"))
  "Returns tokens."
  (run-penelope-tokenizer sentence :model model))

;;(get-penelope-tokens "Paul kicked the ball. Mary caught it.")

(defun run-penelope-sentence-tokenizer (sentences &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentences)
    (error "The function <run-penelope-sentence-tokenizer> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/stok" (encode-json-to-string-for-shell `((:text . ,sentences) (:model . ,model))))))
    (rest (first server-result))))

(defun get-penelope-sentence-tokens (sentences &key (model "en"))
  "Returns sentences."
  (run-penelope-sentence-tokenizer sentences :model model))

;;(run-penelope-sentence-tokenizer "Paul kicked the ball. Mary caught it.")


(defun run-penelope-text-tokenizer (texts &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (listp texts)
    (error "The function <run-penelope-text-tokenizer> expects a list as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/sents_toks" (encode-json-to-string-for-shell `((:texts . ,texts) (:model . ,model))))))
    server-result))

(defun get-penelope-text-tokens (texts &key (model "en"))
  "Returns for every text, a list with sentences, which are on its turn a list of tokens."
  (run-penelope-text-tokenizer texts :model model))

(get-penelope-text-tokens '("This is one article. And it has two sentences"
                            "Then there is a second article. It talks about Mr. Smith."))

;;(run-penelope-sentence-tokenizer "Paul kicked the ball. Mary caught it.")



;;;;;;;;;;;;;;;;;;;;;
;; Lemmatizer  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-lemmatizer (sentence &key (model "en"))
  "Call the penelope server to get the lemmatize the words in the input sentence."
  (unless (stringp sentence)
    (if (listp sentence)
      (setf sentence (format nil "~{~a~^ ~}" sentence))
      (error "The function <run-penelope-lemmatizer> expects a string as input")))

  (curl-json "https://www.fcg-net.org/penelope/nlp/lemma" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model)))))

;;(run-penelope-lemmatizer "April is the fourth month of the year")

(defun get-penelope-lemmas (sentence &key (model "en"))
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a noun chunk."
    (rest (assoc :lemmas (run-penelope-lemmatizer sentence :model model))))

;;(get-penelope-lemmas "April is the fourth month of the year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word embeddings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-sentence-word-embeddings (sentence &key (model "en") (source nil))
  "Call the penelope server to get the word embeddings of a single sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-sentence-word-embeddings> expects a string as input"))
  (let ((server-result
         (if (eq source 'guardian-data)
           (curl-json "https://www.fcg-net.org/penelope/word2vec/vec" (encode-json-to-string-for-shell `((:text . ,sentence))))
           (curl-json "https://www.fcg-net.org/penelope/nlp/glove" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model)))))))

    (if (eq source 'guardian-data)
      (cond ((null (search " " sentence)) ;;single word
             (list server-result))
            ((equalp (type-of server-result) 'cl-user::simple-text-string) ;;"word 'x' not in vocabulary"
             ;;remove out-of-vocabulary word
             (let* ((server-result (cdr (assoc :result (decode-json-from-string
                                                        (format nil "{\"result\":~a}"
                                                                (cl-ppcre:regex-replace-all "\\" server-result "\\\\"))))))
                    (out-of-voc-word-array (second (multiple-value-list (cl-ppcre:scan-to-strings "'(.+)'" server-result))))
                    (out-of-voc-word (when out-of-voc-word-array (first (array->list out-of-voc-word-array)))))
               (format t "Out of vocabulary word in Guardian word embeddings: ~a ~%" out-of-voc-word)
               (setf sentence (string-replace sentence out-of-voc-word ""))
               (setf sentence (list-of-strings->string (split-sequence:split-sequence #\Space sentence :remove-empty-subseqs t)))
               (if sentence
                 (run-penelope-sentence-word-embeddings sentence :source source)
                 nil)))
            (t
             (rest (first server-result))))
      (rest (first server-result)))))
  
;(run-penelope-sentence-word-embeddings "lock battle 2022à" :source 'glove)

(defun get-word-embeddings (sentence &key (source 'glove) (lemmatize? nil)) ;;or guardian-data
  "Get the word embeddings for a sentence in a '((word1 vector1) (word2 vector2)) format."
  (when lemmatize?
    (setf sentence (list-of-strings->string (get-penelope-lemmas sentence))))
  (let ((penelope-embeddings (run-penelope-sentence-word-embeddings sentence :source source)))
    
    (loop for word-embedding in penelope-embeddings
          for token = (rest (assoc :token word-embedding))
          for vector = (rest (assoc :vector word-embedding))
          collect (list token vector))))

;(cosine-similarity (second (first (get-word-embeddings "girl" :source 'glove)))
;                   (second (first (get-word-embeddings "girls" :source 'glove))))

;(get-word-embeddings "hello world" :source 'guardian-data)

(defun get-word-similarity (word1 word2 &key (source 'glove) (lemmatize? nil)) 
  "Calculates the cosine similarity between two words based on the word embeddings from Glove."
  (let ((vector1 (second (first (get-word-embeddings word1 :source source :lemmatize? lemmatize?))))
        (vector2 (second (first (get-word-embeddings word2 :source source :lemmatize? lemmatize?)))))
    (cosine-similarity vector1 vector2)))

;;(get-word-similarity "boy" "banana" :source 'glove)

(defun get-phrase-similarity (phrase1 phrase2 &key (source 'glove) (lemmatize? nil))
  ;;multiply? pretty girl vs handsome boy
  (let* ((vectors-for-phrase-1
          (mapcar #'(lambda (word)
                      (second (first (get-word-embeddings word :source source :lemmatize? lemmatize?))))
                  (split-sequence:split-sequence #\Space phrase1)))
         (vector1 (utils::multiply-list-of-vectors vectors-for-phrase-1))
         (vectors-for-phrase-2
          (mapcar #'(lambda (word)
                      (second (first (get-word-embeddings word :source source :lemmatize? lemmatize?))))
                  (split-sequence:split-sequence #\Space phrase2)))
         (vector2 (utils::multiply-list-of-vectors vectors-for-phrase-2)))
    (utils::cosine-similarity vector1 vector2)))

;;(get-phrase-similarity "Mickey Mouse" "Trump" :source 'glove :lemmatize? nil)
;;(get-phrase-similarity "handsome boy" "pretty girl" :source 'glove)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preprocessing sentences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(sentence->bag-of-words))

(defun sentence->bag-of-words (sentence &key lemmatize? (language "en"))
  "Given a sentence as a string, this function turns it into a list of
words (strings), optionally lemmatized. It does not conserve
punctuation marks."
    ;;1) remove punctuation
    (setf sentence
          (coerce (loop for char across sentence
                        unless (member char '(#\, #\. #\" #\; #\- #\Tab #\Newline ;#\•
                                                  ))
                        collect char) 'string))

    ;;2) lemmatize if needed, otherwise just tokenize
    (if lemmatize?
      (setf sentence
            (get-penelope-lemmas sentence :model language))
      (setf sentence
            (get-penelope-tokens sentence :model language)))

    sentence)

