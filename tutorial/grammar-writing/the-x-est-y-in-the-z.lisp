
;;Special issue Thomas Hoffmann, Creativity
;;Contribution by Paul Van Eecke and Katrien Beuls
;;###################################################

;;This file contains a grammar fragment for the construct "He's not the sharpest tool in the box".
;; It demonstrates how creative use can also lead to "He's not the crunchiest chip in the bag".


;; (ql:quickload :type-hierarchies)

(in-package :type-hierarchies)
(activate-monitor trace-fcg)


;; Let's first define a type-hierarchy that expresses certain properties of certain words

(progn
  (defparameter *type-hierarchy* (make-instance 'type-hierarchy))

  (add-categories '(sharp tool box quick bunny forest smart suit wardrobe crunchy chip bag) *type-hierarchy*)
  (add-categories '(object container positive-property) *type-hierarchy*)

  (add-link 'tool 'object *type-hierarchy*)
  (add-link 'bunny 'object *type-hierarchy*)
  (add-link 'suit 'object *type-hierarchy*)
  (add-link 'chip 'object *type-hierarchy*)

  (add-link 'box 'container *type-hierarchy*)
  (add-link 'forest 'container *type-hierarchy*)
  (add-link 'wardrobe 'container *type-hierarchy*)
  (add-link 'bag 'container *type-hierarchy*)
  
  (add-link 'sharp 'positive-property *type-hierarchy*)
  (add-link 'quick 'positive-property *type-hierarchy*)
  (add-link 'smart 'positive-property *type-hierarchy*)
  (add-link 'crunchy 'positive-property *type-hierarchy*)

  (add-link 'object 'tool *type-hierarchy*)
  (add-link 'object 'bunny *type-hierarchy*)
  (add-link 'object 'suit *type-hierarchy*)
  (add-link 'object 'chip *type-hierarchy*)

  (add-link 'container 'box *type-hierarchy*)
  (add-link 'container 'forest *type-hierarchy*)
  (add-link 'container 'wardrobe *type-hierarchy*)
  (add-link 'container 'bag *type-hierarchy*)

  (add-link 'positive-property 'sharp *type-hierarchy*)
  (add-link 'positive-property 'quick *type-hierarchy*)
  (add-link 'positive-property 'smart *type-hierarchy*)
  (add-link 'positive-property 'crunchy *type-hierarchy*))


(def-fcg-constructions-with-type-hierarchy the-x-est-y-in-the-z
  :cxn-inventory *the-x-est-y-in-the-z*
  :type-hierarchy *type-hierarchy*
  (def-fcg-cxn he-cxn
               ((?he-unit
                 (args (?x))
                 (sem-cat (sem-function referent))
                 (syn-cat (lex-class pronoun)))
                <-
                (?he-unit
                 (HASH meaning ((male-person ?x)))
                 --
                 (HASH form ((string ?he-unit "he"))))))

  (def-fcg-cxn copula-cxn
               ((?clause-unit
                 (sem-cat (sem-function proposition))
                 (syn-cat (lex-class clause))
                 (subunits (?subject-unit ?copula-unit ?predicate-unit)))
                <-
                (?subject-unit
                 (args (?x))
                 (sem-cat (sem-function referent))
                 --
                 (syn-cat (lex-class pronoun)))
                (?copula-unit
                 --
                 (HASH form ((string ?copula-unit "'s"))))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (left-most-unit ?left-most-unit)
                 --
                 (syn-cat (phrase-type predicate)))
                (?clause-unit
                 --
                 (HASH form ((meets ?subject-unit ?copula-unit)
                             (meets ?copula-unit ?left-most-unit))))))

  (def-fcg-cxn sharpest-cxn
               ((?sharpest-unit
                 (syn-cat (lex-id sharp)))
                <-
                (?sharpest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type sharp)
                          (sem-field hardware))
                 --
                 (HASH form ((string ?sharpest-unit "sharpest"))))))

  (def-fcg-cxn quickest-cxn
               ((?quickest-unit
                 (syn-cat (lex-id quick)))
                <-
                (?quickest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type quick)
                          (sem-field animals))
                 --
                 (HASH form ((string ?quickest-unit "quickest"))))))

  (def-fcg-cxn smartest-cxn
               ((?smartest-unit
                 (syn-cat (lex-id smart)))
                <-
                (?smartest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type smart)
                          (sem-field clothing))
                 --
                 (HASH form ((string ?smartest-unit "smartest"))))))

  (def-fcg-cxn crunchiest-cxn
               ((?crunchiest-unit
                 (syn-cat (lex-id crunchy)))
                <-
                (?crunchiest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type crunchy)
                          (sem-field food))
                 --
                 (HASH form ((string ?crunchiest-unit "crunchiest"))))))

  (def-fcg-cxn tool-cxn
               ((?tool-unit
                 (syn-cat (lex-id tool)))
                <-
                (?tool-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type tool)
                          (sem-field hardware))
                 --
                 (HASH form ((string ?tool-unit "tool"))))))

  (def-fcg-cxn bunny-cxn
               ((?bunny-unit
                 (syn-cat (lex-id bunny)))
                <-
                (?bunny-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type bunny)
                          (sem-field animals))
                 --
                 (HASH form ((string ?bunny-unit "bunny"))))))

  (def-fcg-cxn suit-cxn
               ((?suit-unit
                 (syn-cat (lex-id bunny)))
                <-
                (?suit-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type suit)
                          (sem-field clothing))
                 --
                 (HASH form ((string ?suit-unit "suit"))))))

  (def-fcg-cxn chip-cxn
               ((?chip-unit
                 (syn-cat (lex-id chip)))
                <-
                (?chip-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type chip)
                          (sem-field food))
                 --
                 (HASH form ((string ?chip-unit "chip"))))))

  (def-fcg-cxn box-cxn
               ((?box-unit
                 (syn-cat (lex-id box)))
                <-
                (?box-unit
                 (sem-cat (sem-class object)
                          (sem-type box)
                          (sem-field hardware))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?box-unit "box"))))))

  (def-fcg-cxn forest-cxn
               ((?forest-unit
                 (syn-cat (lex-id forest)))
                <-
                (?forest-unit
                 (sem-cat (sem-class object)
                          (sem-type forest)
                          (sem-field animals))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?forest-unit "forest"))))))

  (def-fcg-cxn wardrobe-cxn
               ((?wardrobe-unit
                 (syn-cat (lex-id wardrobe)))
                <-
                (?wardrobe-unit
                 (sem-cat (sem-class object)
                          (sem-type wardrobe)
                          (sem-field clothing))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?wardrobe-unit "wardrobe"))))))

  (def-fcg-cxn bag-cxn
               ((?bag-unit
                 (syn-cat (lex-id bag)))
                <-
                (?bag-unit
                 (sem-cat (sem-class object)
                          (sem-type bag)
                          (sem-field food))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?bag-unit "bag"))))))
  
  (def-fcg-cxn the-cxn
               ((?the-unit
                 (syn-cat (lex-class article)
                          (lex-id the)))
                <-
                (?the-unit
                 (sem-cat (sem-function determiner))
                 --
                 (HASH form ((string ?the-unit "the"))))))

  (def-fcg-cxn in-cxn
               ((?in-unit
                 (syn-cat (lex-class in-preposition)
                          (lex-id in)))
                <-
                (?in-unit
                 (sem-cat (sem-class relation))
                 --
                 (HASH form ((string ?in-unit "in"))))))

  (def-fcg-cxn not-cxn
               ((?not-unit
                 (syn-cat (lex-class not-negation)
                          (lex-id not)))
                <-
                (?not-unit
                 (sem-cat (sem-function negation))
                 --
                 (HASH form ((string ?not-unit "not"))))))

  (def-fcg-cxn not-the-sharpest-tool-in-the-box-cxn
               ((?not-unit
                 (sem-cat (sem-function negation)))
                (?the-1-unit
                 (sem-cat (sem-function determiner)))
                (?sharpest-unit
                 (sem-cat (sem-class modifier)
                          (sem-type positive-property)
                          (sem-field hardware)))
                (?tool-unit
                 (subunits (?sharpest-unit ?the-1-unit))
                 (sem-cat (sem-class object)
                          (sem-type object)
                          (sem-field hardware)))
                (?in-unit
                 (sem-cat (sem-class relation)))
                (?the-2-unit
                 (sem-cat (sem-function determiner)))
                (?box-unit
                 (sem-cat (sem-class object)
                          (sem-type container)
                          (sem-field hardware))
                 (subunits (?in-unit ?the-2-unit)))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (syn-cat (phrase-type predicate))
                 (subunits (?not-unit ?tool-unit ?box-unit))
                 (left-most-unit ?not-unit))
                <-
                (?not-unit
                 --
                 (syn-cat (lex-class not-negation)))
                (?the-1-unit
                 --
                 (syn-cat (lex-class article)))
                (?sharpest-unit
                 --
                 (syn-cat (lex-class adjective)
                          (lex-type superlative)
                          (lex-id sharp)))
                (?tool-unit
                 --
                 (syn-cat (lex-class noun)
                          (lex-id tool)))
                (?in-unit
                 --
                 (syn-cat (lex-class in-preposition)))
                (?the-2-unit
                 --
                 (syn-cat (lex-class article)))
                (?box-unit
                 --
                 (syn-cat (lex-class noun)
                          (lex-id box)))
                (?predicate-unit
                 (HASH meaning ((property not-smart ?x)
                                (metaphorical-expression ?metaphor ?x)
                                (semantic-field ?metaphor hardware)))
                 --
                 (HASH form ((meets ?not-unit ?the-1-unit)
                             (meets ?the-1-unit ?sharpest-unit)
                             (meets ?sharpest-unit ?tool-unit)
                             (meets ?tool-unit ?in-unit)
                             (meets ?in-unit ?the-2-unit)
                             (meets ?the-2-unit ?box-unit))))))
  
  (def-fcg-cxn not-the-x-est-y-in-the-z-cxn
               ((?not-unit
                 (sem-cat (sem-function negation)))
                (?the-1-unit
                 (sem-cat (sem-function determiner)))
                (?x-est-unit
                 (sem-cat (sem-class modifier)
                          (sem-type positive-property)
                          (sem-field ?sem-field)))
                (?y-unit
                 (subunits (?x-est-unit ?the-1-unit))
                 (sem-cat (sem-class object)
                          (sem-type object)
                          (sem-field ?sem-field)))
                (?in-unit
                 (sem-cat (sem-class relation)))
                (?the-2-unit
                 (sem-cat (sem-function determiner)))
                (?z-unit
                 (sem-cat (sem-class object)
                          (sem-type container)
                          (sem-field ?sem-field))
                 (subunits (?in-unit ?the-2-unit)))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (syn-cat (phrase-type predicate))
                 (subunits (?not-unit ?y-unit ?z-unit))
                 (left-most-unit ?not-unit))
                <-
                (?not-unit
                 --
                 (syn-cat (lex-class not-negation)))
                (?the-1-unit
                 --
                 (syn-cat (lex-class article)))
                (?x-est-unit
                 --
                 (syn-cat (lex-class adjective)
                          (lex-type superlative)))
                (?y-unit
                 --
                 (syn-cat (lex-class noun)))
                (?in-unit
                 --
                 (syn-cat (lex-class in-preposition)))
                (?the-2-unit
                 --
                 (syn-cat (lex-class article)))
                (?z-unit
                 --
                 (syn-cat (lex-class noun)))
                (?predicate-unit
                 (HASH meaning ((property not-smart ?x)
                                (metaphorical-expression ?metaphor ?x)
                                (semantic-field ?metaphor ?sem-field)))
                 --
                 (HASH form ((meets ?not-unit ?the-1-unit)
                             (meets ?the-1-unit ?x-est-unit)
                             (meets ?x-est-unit ?y-unit)
                             (meets ?y-unit ?in-unit)
                             (meets ?in-unit ?the-2-unit)
                             (meets ?the-2-unit ?z-unit)))))))


;; The sharpest tool in the box.
(comprehend "he 's not the sharpest tool in the box" :cxn-inventory *the-x-est-y-in-the-z*)

;; The quickest bunny in the forest.
(comprehend "he 's not the quickest bunny in the forest" :cxn-inventory *the-x-est-y-in-the-z*)

;; The smartest suit in the wardrobe.
(formulate '((male-person x)
             (property not-smart x)
             (metaphorical-expression metaphor x)
             (semantic-field metaphor clothing)) :cxn-inventory *the-x-est-y-in-the-z*)

;; The crunchiest chip in the bag.
(formulate '((male-person x)
             (property not-smart x)
             (metaphorical-expression metaphor x)
             (semantic-field metaphor food)) :cxn-inventory *the-x-est-y-in-the-z*)

