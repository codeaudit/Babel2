(in-package :nlp-tools)

(export '(proper-noun-p
          decapitalize-string
          sentence-cosine-similarity))

(defun proper-noun-p (word &optional (language-model "en"))
  "Calls Penelope POS tagger to verify if the word is a proper noun."
  (let ((pos-tag
         (rest (assoc :tag (first (run-penelope-pos-tagger word :model language-model))))))
    (when (member pos-tag '("NNP" "NNPS") :test #'equalp)
      t)))

(defun decapitalize-string (sentence &key (language-model "en"))
  "Decapitalize a the first word of a string. Takes into account proper nouns in the first position of the sentence (does not decapitalize them)."
  (if (or
       (lower-case-p (char sentence  0)) ;;first word is lowercase
       (proper-noun-p (first-word sentence) language-model) ;;first word is a proper noun
       (equalp (first-word sentence) "I"))
    sentence ;;keep as it is
    (replace sentence (downcase sentence) :start1 0 :end1 1 :start2 0 :end2 1)))

(defun get-sentence-embedding (sentence &key (source 'glove) (lemmatize? nil))
  (utils::multiply-list-of-vectors
   (mapcar #'second (get-word-embeddings sentence :source source :lemmatize? lemmatize?))))

(defun sentence-cosine-similarity (sentence1 sentence2)
  (cosine-similarity (get-sentence-embedding sentence1)
                     (get-sentence-embedding sentence2)))

;;;;; Penelope Dependency Parser utilities
;;;;; --------------------------------------------------------------------------------
;;;;; Utilities for accessing and manipulating dependency-parser-results.

(export '(dp-get-spec dp-get-head-id dp-get-token dp-get-node-id dp-get-tag dp-get-dependency
                      dp-build-utterance-from-dependency-tree dp-build-utterance-as-list-from-dependency-tree
                      dp-combine-tokens-in-dependency-analysis))

(defun dp-get-spec (keyword dependency-tree-constituent)
  (rest (assoc keyword dependency-tree-constituent)))

(defun dp-get-head-id (dependency-tree-constituent)
  (dp-get-spec :head--id dependency-tree-constituent))

(defun dp-get-token (dependency-tree-constituent)
  (dp-get-spec :token dependency-tree-constituent))

(defun dp-get-node-id (dependency-tree-constituent)
  (dp-get-spec :node--id dependency-tree-constituent))

(defun dp-get-tag (dependency-tree-constituent)
  (dp-get-spec :tag dependency-tree-constituent))

(defun dp-get-dependency (dependency-tree-constituent)
  (dp-get-spec :dependency dependency-tree-constituent))

(defun dp-build-utterance-as-list-from-dependency-tree (dependency-tree)
  "Returns the ordered list of strings of an utterance from a dependency tree analysis."
  (mapcar #'(lambda(dependent)
              (rest (assoc :token dependent)))
          dependency-tree))
;; (dp-build-utterance-as-list-from-dependency-tree (get-penelope-dependency-analysis "John's book"))
;; -> ("John" "'s" "book")

(defun dp-build-utterance-from-dependency-tree (dependency-tree)
  "Builds an utterance as a single string from the dependency tree analysis."
  (list-of-strings->string (dp-build-utterance-as-list-from-dependency-tree dependency-tree)
                           :separator " "))
;; (dp-build-utterance-from-dependency-tree (get-penelope-dependency-analysis "John's book"))
;; -> "John 's book"

(defun dp-combine-tokens-in-dependency-analysis (list-of-strings dependency-tree &optional replacement-string)
  "Replace a sequence of strings with a single string, using the tags from the last word for the analysis."
  (unless replacement-string (setf replacement-string (list-of-strings->string list-of-strings :separator " ")))
  (if (loop for string in list-of-strings ;; Check for the same sequence of strings.
            for leaf in dependency-tree
            do (unless (string= string (dp-get-token leaf))
                 (return nil))
            finally (return t))
    ;; If found, we throw away all but the last leaf, and replace its string.
    (cons (loop for spec in (nth (1- (length list-of-strings)) dependency-tree)
                collect (if (eql :token (first spec))
                          (cons :token replacement-string)
                          spec))
          (subseq dependency-tree (length list-of-strings)))
    ;; If not, then we continue recursively with the rest of the dependency tree.
    (cons (first dependency-tree)
          (dp-combine-tokens-in-dependency-analysis list-of-strings (rest dependency-tree) replacement-string))))
