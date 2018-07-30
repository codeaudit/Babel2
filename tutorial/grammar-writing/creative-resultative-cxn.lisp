(ql:quickload "fcg")

;;Special issue Thomas Hoffmann, Creativity
;;Contribution by Paul Van Eecke and Katrien Beuls
;;###################################################

;;This file contains a grammar fragment for the sentence "Firefighters
;;cut the man free". It demonstrates how constructions are combined in
;;FCG and how the grammar licenses a use of cutting in which the
;;object is unexpressed.

(in-package :fcg)
(def-fcg-constructions hoffmann-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network)
                       (:priority-mode . :depth-first)
                       (:queue-mode . :depth-first-avoid-duplicates)
                        )
  
(def-fcg-cxn resultative-cxn
             ((?verb-unit
               (subunits (?subject-unit ?object-unit ?oblique-unit)))
              <-
              (?subject-unit
               (referent ?agent)
               --
               (boundaries (leftmost-unit ?leftmost-subject-unit)
                           (rightmost-unit ?rightmost-subject-unit))
               (syn-cat (lex-class noun) ;;np
                        (agreement (person ?person)
                                   (number ?number))))
              (?verb-unit
               (sem-valence (agent ?agent))
               (referent ?resultative-event)
               (HASH meaning ((event-frame resultative ?resultative-event)
                              (resultative-agent ?resultative-event ?agent)
                              (resultative-patient ?resultative-event ?patient)
                              (resultative-state ?resultative-event ?result)))
               --
               (syn-cat (lex-class verb)
                        (agreement (person ?person)
                                   (number ?number)))
               (HASH form ((meets ?rightmost-subject-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-object-unit)
                           (meets ?rightmost-object-unit ?oblique-unit))))
              (?object-unit
               (referent ?patient)
               (syn-cat (phrase-type np))
               --
               (boundaries (leftmost-unit ?leftmost-object-unit)
                           (rightmost-unit ?rightmost-object-unit))
               (syn-cat (phrase-type np)))
              (?oblique-unit
               (referent ?result)
               (sem-cat (sem-class modifier))
               --
               (syn-cat (lex-class adjective))))
             :description "Agent causes Patient to become State by V-ing")

(def-fcg-cxn cut-cxn
             ((?cut-unit
               (syn-cat (lex-class verb)
                        (agreement (person ?p)
                                   (number ?nb)))
               (args (?cut-event ?cutter ?cut-object))
               (referent ?cut-event)
               (syn-valence (subject ?subj)
                            (direct-object ?dir-obj))
               (sem-valence (agent ?cutter)
                            (undergoer ?cut-object)))
              <-
              (?cut-unit
               (HASH meaning ((event-frame cut ?cut-event)
                              (cutter ?cut-event ?cutter)
                              (cut.object ?cut-event ?cut-object)))
               --
               (HASH form ((string ?cut-unit "cut")))))
             :cxn-set lex)

(def-fcg-cxn firefighter-cxn
             ((?firefighter-unit
               (referent ?ref)
               (syn-cat (lex-class noun)
                        (agreement (person 3)
                                   (number ?nb))))
              <-
              (?firefighter-unit
               (HASH meaning ((firefighter ?ref)))
               --
               (HASH form ((string ?firefighter-unit "firefighter")))))
             :cxn-set lex)

(def-fcg-cxn plural-n-cxn
             ((?noun-unit
               (subunits (?suffix-unit))
               (boundaries (leftmost-unit ?noun-unit)
                           (rightmost-unit ?suffix-unit)))
              <-
              (?noun-unit
               (syn-cat (lex-class noun)
                        (agreement (person 3)
                                   (number pl)))
               --
               (syn-cat (lex-class noun)))
              (?suffix-unit
               --
               (HASH form ((string ?suffix-unit "-s")
                           (meets ?noun-unit ?suffix-unit)))))
             :cxn-set lex)

(def-fcg-cxn definite-np-cxn
             ((?np-unit
               (referent ?ref)
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (agreement (person ?person)
                                   (number ?number)))
               (boundaries (leftmost-unit ?art-unit)
                           (rightmost-unit ?noun-unit))
               (subunits (?art-unit ?noun-unit)))
              <-
              (?art-unit
               
               (HASH meaning ((definite ?ref)))
               --
               (HASH form ((string ?art-unit "the"))))
              (?noun-unit
               (referent ?ref)
               --
               (syn-cat (lex-class noun)
                        (agreement (person ?person)
                                   (number ?number))))
              (?np-unit
               --
               (HASH form ((meets ?art-unit ?noun-unit))))))

(def-fcg-cxn man-sg-cxn
             ((?man-unit
               (referent ?ref)
               (syn-cat (lex-class noun)
                        (agreement (person 3)
                                   (number sg))))
              <-
              (?man-unit
               (HASH meaning ((man ?ref)))
               --
               (HASH form ((string ?man-unit "man")))))
             :cxn-set lex)

(def-fcg-cxn free-cxn
             ((?free-unit
               (referent ?ref)
               (syn-cat (lex-class adjective))
               (sem-cat (sem-class modifier)))
              <-
              (?free-unit
               (HASH meaning ((free ?ref)))
               --
               (HASH form ((string ?free-unit "free")))))
             :cxn-set lex))

(activate-monitor trace-fcg)

(comprehend-and-formulate "firefighter -s cut the man free")

