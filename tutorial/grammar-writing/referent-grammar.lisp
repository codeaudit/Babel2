(in-package :fcg)

;;This grammar fragment demonstrates how the same meaning network can
;;be expressed in multiple ways, depending on the referent that is
;;chosen, and sometimes even with the same referent.

(def-fcg-constructions referent-grammar
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network)
                       (:priority-mode . :depth-first)
                       (:queue-mode . :depth-first-avoid-duplicates))
  
(def-fcg-cxn bake-lex
             ((?bake-unit
               (referent ?ref)
               (args (frame ?frame)
                     (event ?event))
               (sem-valence (actor ?baker)
                            (undergoer ?baked)))
               <-
               (?bake-unit
                (HASH meaning ((frame bake ?frame ?event)
                               (slot baker ?frame ?baker)
                               (slot baked ?frame ?baked)))
                --
                (syn-cat (lex-id bake)))))

(def-fcg-cxn bart-lex
             ((?bart-unit
              ;; (referent ?bart)
              (args (xyz ?bart))
              (syn-cat (lex-class proper-noun)
                        (agreement 3sg)
                        (phrase-type np))
               (sem-cat (sem-type person)))
              <-
              (?bart-unit
               (HASH meaning ((person bart ?bart)))
               --
               (HASH form ((string ?bart-unit "bart"))))))

(def-fcg-cxn bakes-morph
             ((?bakes-unit
               (referent ?event)
               (syn-cat (lex-class verb)
                        (agreement 3sg)))
              <-
              (?bakes-unit
               (args (event ?event))
               (syn-cat (lex-id bake))
               (HASH meaning ((referent ?event)))
               --
               (HASH form ((string ?bakes-unit "bakes"))))))

(def-fcg-cxn intransitive-cxn
             ((?event-unit
               (subunits (?subject-unit))
               (syn-valence (subject ?subject-unit)))
              <-
              (?subject-unit
               (args (xyz ?agent))
               (syn-cat (phrase-type np))
               --
               (syn-cat (phrase-type np)
                        (agreement ?agr)))
              
              (?event-unit
               (args (event ?event))
               (sem-valence (actor ?agent))
               --
               (HASH form ((meets ?subject-unit ?event-unit))
               (syn-cat (lex-class verb)
                        (agreement ?agr))))))

#|(def-fcg-cxn baker-morph
             ((?baker-unit
               (footprints (morph))
               (referent ?baker)
               (syn-cat (lex-class noun)))
              <-
              (?baker-unit
               (footprints (not morph))
               (args (?ev ?baker ?baked))
               (syn-cat (lex-id bake))
             ;;  (HASH referent ?baker)
               --
               (HASH form ((string ?baker-unit "baker")))))
             :disable-automatic-footprints t)


(def-fcg-cxn the-baking-morph
             ((?the-baking-unit
               (footprints (morph))
               (referent ?the-baking)
               (syn-cat (lex-class noun)
                        (phrase-type np)
                        (is-nominalization +)))
              <-
              (?the-baking-unit
               (footprints (not morph))
               (args (?the-baking ?baker ?baked))
               (syn-cat (lex-id bake))
               (HASH referent ?the-baking)
              ; (HASH meaning ((referent-status definite ?the-baking ?baking)))
               --
               (HASH form ((string ?the-unit "the")
                           (string ?the-baking-unit "baking")
                           (meets ?the-unit ?the-baking-unit)))))
             :disable-automatic-footprints t)

(def-fcg-cxn a-cake-phrase
             ((?a-cake-unit
               (referent ?a-cake)
               (args (?a-cake ?cake ?ctx))
               (syn-cat (phrase-type np)
                        (agreement 3sg))
               (subunits (?a-string)))
              (?a-string
               (syn-cat (lex-class article)
                        (indefinite +)))
              <-
              (?a-cake-unit
               (HASH meaning ((referent-status indefinite ?a-cake ?cake)
                              (object cake ?cake ?ctx)))
               --
               (HASH form ((string ?a-string "a")
                           (string ?a-cake-unit "cake")
                           (meets ?a-string ?a-cake-unit))))))

(def-fcg-cxn he-lex
             ((?he-unit
               (referent ?ref))
              <-
              (?he-unit
               (HASH meaning ((discourse-status identifiable-male-referent ?ref)))
               --
               (syn-cat (lex-id he)
                        (phrase-type np)
                        (agreement 3sg)))))

(def-fcg-cxn he-morph
             ((?he-unit
               (syn-cat (case nom)))
              <-
              (?he-unit
               (syn-cat (lex-id he)
                        (phrase-type np)
                        (agreement 3sg))
               --
               (HASH form ((string ?he-unit "he"))))))

(def-fcg-cxn him-morph
             (
              <-
              (?him-unit
               (syn-cat (lex-id he)
                        (phrase-type np)
                        (agreement 3sg)
                        (case non-nom))
               --
               (HASH form ((string ?him-unit "him"))))))


(def-fcg-cxn active-transitive-cxn
             ((?event-unit
               (subunits (?subject-unit ?object-unit))
               (syn-valence (subject ?subject-unit)
                            (direct-object ?object-unit)))
              <-
              (?subject-unit
               (referent ?subj)
               (syn-cat (phrase-type np)
                        (agreement ?agr))
               --
               (syn-cat (phrase-type np)
                        (agreement ?agr)))
              (?object-unit
               (referent ?obj)
               (syn-cat (phrase-type np))
               --
               (syn-cat (phrase-type np)))
              (?event-unit
               (referent ?ev)
               (sem-valence (actor ?subj)
                            (undergoer ?obj))
               --
               (HASH form ((precedes ?subject-unit ?event-unit)
                           (precedes ?event-unit ?object-unit)))
               (syn-cat (lex-class verb)
                        (agreement ?agr)))))

;;the cake was baked by him, the baking of the cake by him
(def-fcg-cxn passive-agent-cxn
             ((?event-unit
               (subunits (?agent-unit)))
              (?agent-unit
               (syn-cat (case non-nom)))
               ;(subunits (?by-unit)))
              <-
              (?event-unit
               (sem-valence (actor ?agent-ref))
               (syn-cat (is-nominalization +))
               --
               (syn-cat (is-nominalization +))) ;;TO DO: also for passives!!!
              (?agent-unit
               (referent ?agent-ref)
               --
               (HASH form ((precedes ?event-unit ?by-unit)
                           (string ?by-unit "by")
                           (meets ?by-unit ?agent-unit)))
               (syn-cat (phrase-type np)))
               ))
               

(def-fcg-cxn nominalized-event-with-object-cxn
             ((?nominalized-unit
               (subunits (?object-unit)))
              (?object-unit
               (subunits (?of-unit)))
              (?of-unit
               (syn-cat (lex-class preposition)))
              
              <-
              (?nominalized-unit
               (referent ?the-baking)
               (args (?the-baking ?baker ?baked))
               (syn-cat (is-nominalization +))
               --
               (syn-cat (is-nominalization +)))
              (?object-unit
               (referent ?baked)
               
               (syn-cat (phrase-type np))
               --
               (HASH form ((string ?of-unit "of")
                           (meets ?nominalized-unit ?of-unit)
                           (meets ?of-unit ?object-unit)))
               (syn-cat (phrase-type np)))))
|#
)

(activate-monitor trace-fcg)

;;bart bakes
(formulate-and-comprehend '((frame bake ?bake-frame ?event)
             (slot baker ?bake-frame ?bart)
             (slot baked ?bake-frame unknown)
             (person bart ?bart)
             (referent ?event)))

;;bart the baker
(formulate '((frame bake ?bake-frame ?event)
             (slot baker ?bake-frame ?bart)
             (slot baked ?bake-frame unknown)
             (person ?bart)
             (referent ?bart)))

;; the baker
(formulate '((frame bake ?bake-frame ?event)
             (slot baker ?bake-frame ?baker)
             (slot baked ?bake-frame unknown)
             (referent ?baker)))

;; the baked
(formulate '((frame bake ?bake-frame ?event)
             (slot baker ?bake-frame unknown)
             (slot baked ?bake-frame ?baked)
             (referent ?baked)))

;; the cake
(formulate '((frame bake ?bake-frame ?event)
             (slot baker ?bake-frame unknown)
             (slot baked ?bake-frame ?cake)
             (edible-object ?cake)
             (referent ?cake)))

;;the baking
(formulate '((frame bake ?bake-frame ?event)
             (slot baker ?bake-frame ?bart)
             (slot baked ?bake-frame unknown)
             (referent ?bake-frame)))






(comprehend "bakes")

(comprehend "baker")

(formulate '((event-frame bake ev) (slot baker ev paul) (slot baked ev cake)) :referent 'ev)

;;the baking of the cake by paul
(formulate '((event-frame bake ev)
             (slot baker ev paul)
             (slot baked ev cake-frame)
             (frame cake-frame cake)
             (referent-status cake-frame definite)
             
             (referent-status ev definite))
              :referent 'ev)

;;the baking of cake
(formulate '((event-frame bake ev)
             (slot baker ev ?baker)
             (slot baked ev cake)
             (referent-status ev definite))
              :referent 'ev)


(comprehend "he bakes a cake")

;;meaning results in two possible utterances:
;;-------------------------------------
(formulate '((discourse-status identifiable-male-referent oliver)
             (event-frame BAKE bake-ev)
             (slot BAKER bake-ev oliver)
             (slot BAKED bake-ev a-cake)
             (referent-status indefinite a-cake cake)
             (object CAKE cake context)) :referent 'oliver)

(comprehend-and-formulate "the baking of a cake")
;;He bakes a cake
;;The baking of a cake by him

