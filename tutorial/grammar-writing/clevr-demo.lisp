
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEMONSTRATION OF FLUID CONSTRUCTION GRAMMAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 1. Load and set up FCG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

;;;; 2. Load the following FCG Example Grammar
;;;;    by executing def-fcg-constructions
;;;;    and def-fcg-cxn definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;"What number of small red cubes are left of the big blue cylinder?"

(def-fcg-constructions clevr-one-hop-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (sem-fields sequence)
                  (footprints set))
  :fcg-configurations ((:production-order lex cxn morph)
                       (:parse-order morph lex cxn)
                       (:de-render-mode . :de-render-string-meets-precedes)
                       (:render-mode . :render-string-meets-precedes)
                       (:form-predicates meets precedes)
                       (:node-tests  :check-duplicate :restrict-nr-of-nodes)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network )
                       (:production-goal-tests :no-applicable-cxns )
                       (:cxn-supplier-mode . :ordered-by-label)
                       ;; For guiding search:
                       (:node-expansion-mode . :default)
                       (:priority-mode . :depth-first)
                       (:queue-mode . :by-priority))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:hide-features ()) 
                                 (:with-search-debug-data . t))
  :hierarchy-features (subunits)
  :hashed nil
  
  ;; Lexical constructions
  (def-fcg-cxn small-cxn
               ((?word-unit
                 (args (?x))
                 (sem-cat (sem-class size))
                 (syn-cat (lex-class adjective)))
                <-
                (?word-unit
                 (HASH meaning ((size small ?x)))                     
                 --
                 (HASH form ((string ?word-unit "small")))))
               :cxn-set lex)

  (def-fcg-cxn big-cxn
               ((?word-unit
                 (args (?x))
                 (sem-cat (sem-class size))
                 (syn-cat (lex-class adjective)))
                <-
                (?word-unit
                 (HASH meaning ((size big ?x)))                     
                 --
                 (HASH form ((string ?word-unit "big")))))
               :cxn-set lex)

  (def-fcg-cxn red-cxn
               ((?word-unit
                 (args (?x))
                 (sem-cat (sem-class color))
                 (syn-cat (lex-class adjective)))
                <-
                (?word-unit
                 (HASH meaning ((color red ?x)))                     
                 --
                 (HASH form ((string ?word-unit  "red")))))
               :cxn-set lex)

  (def-fcg-cxn blue-cxn
               ((?word-unit
                 (args (?x))
                 (sem-cat (sem-class color))
                 (syn-cat (lex-class adjective)))
                <-
                (?word-unit
                 (HASH meaning ((color blue ?x)))                     
                 --
                 (HASH form ((string ?word-unit  "blue")))))
               :cxn-set lex)

  (def-fcg-cxn cube-cxn
               ((?word-unit
                 (args (?x))
                 (sem-cat (sem-class shape))
                 (syn-cat (lex-class noun)))
                <-
                (?word-unit
                 (HASH meaning ((shape cube ?x)))                     
                 --
                 (HASH form ((string ?word-unit  "cube")))))
               :cxn-set lex)

  (def-fcg-cxn cylinder-cxn
               ((?word-unit
                 (args (?x))
                 (sem-cat (sem-class shape))
                 (syn-cat (lex-class noun)))
                <-
                (?word-unit
                 (HASH meaning ((shape cylinder ?x)))                     
                 --
                 (HASH form ((string ?word-unit  "cylinder")))))
               :cxn-set lex)

  (def-fcg-cxn left-cxn
               ((?word-unit
                 (referent ?ref)
                 (args (?x ?y))
                 (sem-cat (sem-class spatial-relation)
                          (sem-type left))
                 (syn-cat (lex-class preposition)))
                <-
                (?word-unit
                 (HASH meaning ((spatial-relation left ?rel)
                                (trajector ?rel ?x)
                                (landmark ?rel ?y)))                   
                 --
                 (HASH form ((string ?word-unit "left of")))))
               :cxn-set lex)

  ;; Grammatical constructions
  ;; NOMINAL -> size+color+shape
  (def-fcg-cxn size+color+shape-cxn
               ((?nominal-unit
                 (args (?x))
                 (sem-cat (sem-fields (size color shape)))
                 (syn-cat (syn-function nominal)
                          (leftmost-unit ?size-unit)
                          (rightmost-unit ?shape-unit))
                 (subunits (?size-unit ?color-unit ?shape-unit)))
                (?size-unit
                 (footprints (nominal)))
                (?color-unit
                 (footprints (nominal)))
                (?shape-unit
                 (footprints (nominal)))
                <-
                (?size-unit
                 (args (?x))
                 (sem-cat (sem-class size))
                 (footprints (not nominal))
                 --
                 (footprints (not nominal))
                 (sem-cat (sem-class size))
                 (syn-cat (lex-class adjective)))
                (?color-unit
                 (args (?x))
                 (sem-cat (sem-class color))
                 (footprints (not nominal))
                 --
                 (footprints (not nominal))
                 (sem-cat (sem-class color))
                 (syn-cat (lex-class adjective)))
                (?shape-unit
                 (args (?x))
                 (sem-cat (sem-class shape))
                 (footprints (not nominal))
                 --
                 (footprints (not nominal))
                 (syn-cat (lex-class noun)))
                (?nominal-unit
                 --
                 (HASH form ((meets ?size-unit ?color-unit)
                             (meets ?color-unit ?shape-unit)))))
               :disable-automatic-footprints t
               :cxn-set cxn)

  (def-fcg-cxn filter-unique-cxn
               ((?determined-noun-phrase-unit
                 (subunits (?determiner-unit ?nominal-unit))
                 (args (?object))
                 (sem-cat (sem-function referring-expression))
                 (syn-cat (phrase-type np)
                          (definite +)
                          (leftmost-unit ?determiner-unit)
                          (rightmost-unit ?rightmost-unit)))
                <-
                (?nominal-unit
                 (args (?object))
                 (sem-cat (sem-fields ?fields))
                 --
                 (syn-cat (syn-function nominal)
                          (leftmost-unit ?leftmost-unit)
                          (rightmost-unit ?rightmost-unit)))
                (?determiner-unit
                 (HASH meaning ((filter-unique ?object ?object-set)))
                 --
                 (HASH form ((string ?determiner-unit "the")
                             (meets ?determiner-unit ?leftmost-unit)))))
               :cxn-set cxn)

  (def-fcg-cxn relate-filter-cxn
               ((?prepositional-phrase-unit
                 (referent ?filtered-object-set)
                 (args (?x ?y))
                 (subunits (?spatial-relation-unit ?determined-noun-phrase-unit))
                 (syn-cat (leftmost-unit ?spatial-relation-unit)
                          (rightmost-unit ?rightmost-np-unit)
                          (phrase-type pp))
                 (sem-cat (sem-function referring-expression)
                          (sem-class spatial-relation)))
                <-
                (?nominal-unit
                 (args (?x))
                 (sem-cat (sem-fields ?fields))
                 --
                 (syn-cat (syn-function nominal)
                          (rightmost-unit ?rightmost-nominal-unit)))
                (?spatial-relation-unit
                 (referent ?filtered-object-set)
                 (args (?x ?y))
                 (sem-cat (sem-class spatial-relation))
                 (HASH meaning ((relate-filter ?filtered-object-set ?x ?y)))
                 --
                 (sem-cat (sem-class spatial-relation))
                 (HASH form ((precedes ?rightmost-nominal-unit ?spatial-relation-unit)
                             (meets ?spatial-relation-unit ?leftmost-np-unit))))
                (?determined-noun-phrase-unit
                 (args (?y))
                 (sem-cat (sem-function referring-expression))
                 --
                 (syn-cat (leftmost-unit ?leftmost-np-unit)
                          (rightmost-unit ?rightmost-np-unit)
                          (phrase-type np))))
               :cxn-set cxn)

  (def-fcg-cxn what-number-of-count-cxn
               ((?plural-noun-phrase-unit
                 (subunits (?nominal-unit ?plural-ending-unit))
                 (syn-cat (phrase-type np)
                          (number plural)
                          (leftmost-unit ?leftmost-nominal-unit)
                          (rightmost-unit ?plural-ending-unit))
                 (sem-cat (sem-function referring-expression)))
                (?question-unit
                 (subunits (?plural-noun-phrase-unit ?prepositional-phrase-unit)))
                <-
                (?question-unit
                 (HASH meaning ((count ?number ?filtered-object-set)))
                 --
                 (HASH form ((string ?question-unit "what number of")
                             (meets ?question-unit ?leftmost-nominal-unit))))
                (?prepositional-phrase-unit
                 (referent ?filtered-object-set)
                 (args (?x ?y))
                 (sem-cat (sem-class spatial-relation))
                 --
                 (sem-cat (sem-class spatial-relation))
                 (syn-cat (phrase-type pp)))
                (?nominal-unit
                 (args (?x))
                 (syn-cat (syn-function nominal))
                 --
                 (syn-cat (syn-function nominal)
                          (leftmost-unit ?leftmost-nominal-unit)
                          (rightmost-unit ?rightmost-nominal-unit))
                 (HASH form ((meets ?rightmost-nominal-unit ?plural-ending-unit))))
                (?plural-ending-unit
                 --
                 (HASH form ((string ?plural-ending-unit "s")))))
               :cxn-set cxn)
  )

 
;;;; 3.  Start comprehending and formulating!
;;;;    Enjoy following up every step in the process in the
;;;;    web interface (open a web browser at http://localhost:8000)!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;"What number of small red cubes are left of the big blue cylinder?"
(clear-page)

(comprehend '("small" "red" "cube"))
(comprehend '("the" "big" "blue" "cylinder"))
(comprehend '("small" "red" "cube" "left of" "the" "big" "blue" "cylinder"))
(comprehend '("what number of" "small" "red" "cube" "s" "are" "left of" "the" "big" "blue" "cylinder"))


(formulate '((size small obj-1) (color red obj-1) (shape cube obj-1)))
(formulate '((size small obj-1) (color red obj-1) (shape cube obj-1) (filter-unique obj-1 object-set-1)))


;;;; 4. What is missing?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A construction for the copula-be or a modified version of the relate-filter-count-cxn

;; Let's try adding the "are" string to the existing
;; relate-filter-count-cxn. To do so, you should modify the following
;; construction:
(def-fcg-cxn relate-filter-count-cxn
               ((?plural-noun-phrase-unit
                 (subunits (?nominal-unit ?plural-ending-unit))
                 (syn-cat (phrase-type np)
                          (number plural)
                          (leftmost-unit ?leftmost-nominal-unit)
                          (rightmost-unit ?plural-ending-unit))
                 (sem-cat (sem-function referring-expression)))
                (?question-unit
                 (subunits (?plural-noun-phrase-unit ?prepositional-phrase-unit)))
                <-
                (?question-unit
                 (HASH meaning ((count ?number ?filtered-object-set)))
                 --
                 (HASH form ((string ?question-unit "what number of")
                             (meets ?question-unit ?leftmost-nominal-unit))))
                (?prepositional-phrase-unit
                 (referent ?filtered-object-set)
                 (args (?x ?y))
                 (sem-cat (sem-class spatial-relation))
                 --
                 (sem-cat (sem-class spatial-relation))
                 (syn-cat (phrase-type pp)))
                (?nominal-unit
                 (args (?x))
                 (syn-cat (syn-function nominal))
                 --
                 (syn-cat (syn-function nominal)
                          (leftmost-unit ?leftmost-nominal-unit)
                          (rightmost-unit ?rightmost-nominal-unit))
                 (HASH form ((meets ?rightmost-nominal-unit ?plural-ending-unit))))
                (?plural-ending-unit
                 --
                 (HASH form ((string ?plural-ending-unit "s")))))
               :cxn-set cxn)
