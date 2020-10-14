#lang racket
(require (only-in plai define-type test print-only-errors))
(require "definitions.rkt")
(require "simplify-fact.rkt")

(define (to-nnf-helper fact inside-not?)
  (match fact
    [(andf l r)
          (if inside-not?
              (orf (to-nnf-helper l #t) (to-nnf-helper r #t))
              (andf (to-nnf-helper l #f) (to-nnf-helper r #f)))]
    [(orf l r) (if inside-not?
                   (andf (to-nnf-helper l #t) (to-nnf-helper r #t))
                   (orf (to-nnf-helper l #f) (to-nnf-helper r #f)))]
    [(notf f) (to-nnf-helper f (not inside-not?))]
    [_ (if inside-not? (notf fact) fact)]))

(define (to-nnf fact) (to-nnf-helper fact #f))

(module+ test 
  (print-only-errors)
  (test (to-nnf (notf (andf
                       (predf 'positive? 'arg0 '#hash())
                       (predf 'exact-integer? 'arg0 '#hash()))))
        (orf (notf (predf 'positive? 'arg0 '#hash()))
             (notf (predf 'exact-integer? 'arg0 '#hash()))))

  (test (to-nnf (andf (notf (andf (predf 'A 'arg0 '#hash())
                                  (notf (predf 'B 'arg0 '#hash()))))
                      (predf 'C 'arg0 '#hash())))
        (andf
         (orf (notf (predf 'A 'arg0 '#hash()))
              (predf 'B 'arg0 '#hash()))
         (predf 'C 'arg0 '#hash())))

  (test (to-nnf (andf (notf (orf (predf 'A 'arg0 '#hash())
                                 (notf (predf 'B 'arg0 '#hash()))))
                      (predf 'C 'arg0 '#hash())))
        (andf
         (andf (notf (predf 'A 'arg0 '#hash())) (predf 'B 'arg0 '#hash()))
         (predf 'C 'arg0 '#hash())))

  (test (to-nnf (orf (notf (andf (predf 'A 'arg0 '#hash())
                                 (notf (predf 'B 'arg0 '#hash()))))
                     (predf 'C 'arg0 '#hash())))
        (orf
         (orf (notf (predf 'A 'arg0 '#hash())) (predf 'B 'arg0 '#hash()))
         (predf 'C 'arg0 '#hash())))

  (test (to-nnf (notf (andf (orf (andf (predf 'A 'arg0 '#hash())
                                       (predf 'B 'arg0 '#hash()))
                                 (predf 'C 'arg0 '#hash()))
                            (notf (andf (notf (predf 'Y 'arg0 '#hash()))
                                        (predf 'Z 'arg0 '#hash()))))))
        (orf
         (andf
          (orf
           (notf (predf 'A 'arg0 '#hash()))
           (notf (predf 'B 'arg0 '#hash())))
          (notf (predf 'C 'arg0 '#hash())))
         (andf (notf (predf 'Y 'arg0 '#hash()))
               (predf 'Z 'arg0 '#hash()))))
  
  (test (to-nnf (orf (andf (predf 'A 'arg0 '#hash())
                           (predf 'B 'arg0 '#hash()))
                     (andf (predf 'Y 'arg0 '#hash())
                           (predf 'Z 'arg0 '#hash()))))
        (orf (andf (predf 'A 'arg0 '#hash())
                   (predf 'B 'arg0 '#hash()))
             (andf (predf 'Y 'arg0 '#hash())
                   (predf 'Z 'arg0 '#hash()))))

  (test (to-nnf (orf (andf (predf 'A 'arg0 '#hash())
                           (predf 'B 'arg0 '#hash()))
                     (andf (iff (predf 'pred1? 'arg0 '#hash())
                                (predf 'pred2? 'arg0 '#hash())
                                (notf (predf 'pred3? 'arg0 '#hash())))
                           (predf 'Z 'arg0 '#hash()))))
        (orf (andf (predf 'A 'arg0 '#hash())
                   (predf 'B 'arg0 '#hash()))
             (andf (iff (predf 'pred1? 'arg0 '#hash())
                        (predf 'pred2? 'arg0 '#hash())
                        (notf (predf 'pred3? 'arg0 '#hash())))
                   (predf 'Z 'arg0 '#hash()))))

  )

(define (nnf->cnf fact)
  (match fact
    [(andf l r) (andf (nnf->cnf l) (nnf->cnf r))]
    [(orf (andf a-l a-r) r) (define r* (nnf->cnf r))
                            (define a-l* (nnf->cnf a-l))
                            (define a-r* (nnf->cnf a-r))
                            (andf (nnf->cnf (orf a-l* r*)) (nnf->cnf (orf a-r* r*)))]
    [(orf l (andf a-l a-r)) (nnf->cnf (orf (andf a-l a-r) l))]
    [_ fact]))

(module+ test
  (test (nnf->cnf (orf (notf (predf 'positive? 'arg0 '#hash()))
                       (notf (predf 'exact-integer? 'arg1 '#hash()))))
        (orf (notf (predf 'positive? 'arg0 '#hash()))
             (notf (predf 'exact-integer? 'arg1 '#hash()))))
  (test (nnf->cnf (andf
                   (orf (notf (predf 'A 'arg0 '#hash()))
                        (predf 'B 'arg0 '#hash()))
                   (predf 'C 'arg0 '#hash())))
        (andf
         (orf (notf (predf 'A 'arg0 '#hash())) (predf 'B 'arg0 '#hash()))
         (predf 'C 'arg0 '#hash())))
  (test (nnf->cnf (orf
                   (orf (notf (predf 'A 'arg0 '#hash())) (predf 'B 'arg0 '#hash()))
                   (predf 'C 'arg0 '#hash())))
        (orf
         (orf (notf (predf 'A 'arg0 '#hash())) (predf 'B 'arg0 '#hash()))
         (predf 'C 'arg0 '#hash())))
  
  (test (nnf->cnf (orf (andf (predf 'A 'arg0 '#hash())
                             (predf 'B 'arg0 '#hash()))
                       (andf (predf 'Y 'arg0 '#hash())
                             (predf 'Z 'arg0 '#hash()))))
        (andf
         (andf (orf (predf 'Y 'arg0 '#hash()) (predf 'A 'arg0 '#hash()))
               (orf (predf 'Z 'arg0 '#hash()) (predf 'A 'arg0 '#hash())))
         (andf (orf (predf 'Y 'arg0 '#hash()) (predf 'B 'arg0 '#hash()))
               (orf (predf 'Z 'arg0 '#hash()) (predf 'B 'arg0 '#hash())))))

  (test (nnf->cnf 
         (orf
          (andf
           (orf
            (notf (predf 'A 'arg0 '#hash()))
            (notf (predf 'B 'arg0 '#hash())))
           (notf (predf 'C 'arg0 '#hash())))
          (andf (notf (predf 'Y 'arg0 '#hash()))
                (predf 'Z 'arg1 '#hash()))))
        (andf
         (andf (orf (notf (predf 'Y 'arg0 '#hash()))
                    (orf (notf (predf 'A 'arg0 '#hash()))
                         (notf (predf 'B 'arg0 '#hash()))))
               (orf (predf 'Z 'arg1 '#hash())
                    (orf (notf (predf 'A 'arg0 '#hash()))
                         (notf (predf 'B 'arg0 '#hash())))))
         (andf (orf
                (notf (predf 'Y 'arg0 '#hash()))
                (notf (predf 'C 'arg0 '#hash())))
               (orf (predf 'Z 'arg1 '#hash())
                    (notf (predf 'C 'arg0 '#hash()))))))
  )

;(define-type CNF-fact
;  [fact (or/c CNF-and-Facts? CNF-or-Facts literal-facts)])

;(define-type CNF-and-Facts
;  [cnf-andf (c CNF-fact?)
;            (i CNF-fact?)] ; maybe tweak this? 

;(define-type CNF-or-Facts
;  [cnf-orf (c (or/c cnf-ors? literal-facts?))
;           (i (or/c cnf-ors? literal-facts?))]
;
;
;(define-type literal-facts
;  [anyf]
;  [nonef]
;  [N/Af]
;  [notf (i literal-facts?)]
;  [complexf (program BasicExpand?)
;            (scope (hash/c symbol? VariableContent?))]
;  [predf (pred symbol?)
;         (name symbol?)
;         (scope (hash/c symbol? VariableContent?))]
;  [iff (tst Facts?)
;       (thn Facts?)
;       (els Facts?)])


(define (check-purely-on-arg-fact fact argument)
  (match fact
    [(orf l r) (and (check-purely-on-arg-fact l argument)  (check-purely-on-arg-fact r argument)) ]
    [(andf l r) (and (check-purely-on-arg-fact l argument)  (check-purely-on-arg-fact r argument))]
    [(predf pred arg scope) #:when (equal? arg argument) #t]
    [(complex-predf pred arg scope innerApp) #:when (equal? arg argument) #t]
    [(anyf) #t]
    [(nonef) #t]
    [(notf i) (check-purely-on-arg-fact i argument)]
    [(iff tst thn els) (and (check-purely-on-arg-fact tst argument)
                            (check-purely-on-arg-fact thn argument)
                            (check-purely-on-arg-fact els argument))]
    [_ #f]))

(define (check-purely-on-arg-literal fact argument)
  (match fact
    [(predf pred arg scope) #:when (equal? arg argument) #t]
    [(complex-predf pred arg scope innerApp) #:when (equal? arg argument) #t]
    [(anyf) #t]
    [(nonef) #t]
    [(notf i) (check-purely-on-arg-literal i argument)]
    [(iff tst thn els) (and (check-purely-on-arg-fact tst argument)
                            (check-purely-on-arg-fact thn argument)
                            (check-purely-on-arg-fact els argument))];both have error (potentially contractable)
    [_ #f]))


(define (check-purely-on-arg-or fact argument)
  (define (inner-pure? inner arg)
    (if (orf? inner)
        (check-purely-on-arg-or inner arg)
        (check-purely-on-arg-literal inner arg)))
  (match fact
    [(orf l r) (and (inner-pure? l argument) (inner-pure? r argument))]))


(define (rele-and-descender fact argument)
  (define (basic-simply fact)
    (match fact
      [(andf inner (anyf)) inner]
      [(andf (anyf) inner) inner]
      [(andf (anyf) (anyf)) (anyf)]
      [_ fact]))
  (match fact
    [(andf l r)
     (define l-result (process-and-or-literal l argument))
     (define r-result (process-and-or-literal r argument))
     (match* (l-result r-result)
       [((related-otherf l-r l-o) (related-otherf r-r r-o))
        (related-otherf (basic-simply (andf l-r r-r)) (basic-simply (andf l-o r-o)))])]))

(define (process-and-or-literal fact argument)
  (cond
    [(andf? fact)
     (rele-and-descender fact argument)]
    [(orf? fact)
     (if (check-purely-on-arg-or fact argument)
         (related-otherf fact (anyf))
         (related-otherf (anyf) fact))]
    [else
     (if (check-purely-on-arg-literal fact argument)
         (related-otherf fact (anyf))
         (related-otherf (anyf) fact))]))



(define (cnf->relevant-fact fact arg)
  (process-and-or-literal fact arg))
;
(module+ test
  (test
   (cnf->relevant-fact (andf
                        (andf (orf (notf (predf 'Y 'arg0 '#hash()))
                                   (orf (notf (predf 'A 'arg0 '#hash()))
                                        (notf (predf 'B 'arg0 '#hash()))))
                              (orf (predf 'Z 'arg1 '#hash())
                                   (orf (notf (predf 'A 'arg0 '#hash()))
                                        (notf (predf 'B 'arg0 '#hash())))))
                        (andf (orf
                               (notf (predf 'Y 'arg0 '#hash()))
                               (notf (predf 'C 'arg0 '#hash())))
                              (orf (predf 'Z 'arg1 '#hash())
                                   (notf (predf 'C 'arg0 '#hash()))))) `arg0)
   (related-otherf
    (andf
     (orf (notf (predf 'Y 'arg0 '#hash()))
          (orf (notf (predf 'A 'arg0 '#hash())) (notf (predf 'B 'arg0 '#hash()))))
     (orf (notf (predf 'Y 'arg0 '#hash())) (notf (predf 'C 'arg0 '#hash()))))
    (andf
     (orf (predf 'Z 'arg1 '#hash()) (orf (notf (predf 'A 'arg0 '#hash())) (notf (predf 'B 'arg0 '#hash()))))
     (orf (predf 'Z 'arg1 '#hash()) (notf (predf 'C 'arg0 '#hash()))))))
 
  (test (cnf->relevant-fact (andf
                             (andf (orf (notf (predf 'Y 'arg0 '#hash()))
                                        (orf (notf (predf 'A 'arg0 '#hash()))
                                             (notf (predf 'B 'arg0 '#hash()))))
                                   (orf (predf 'Z 'arg1 '#hash())
                                        (orf (notf (predf 'A 'arg0 '#hash()))
                                             (notf (predf 'B 'arg0 '#hash())))))
                             (andf (orf
                                    (notf (predf 'Y 'arg0 '#hash()))
                                    (iff (predf 'D 'arg0 '#hash())
                                         (predf 'E 'arg0 '#hash())
                                         (predf 'F 'arg0 '#hash())))
                                   (orf (predf 'Z 'arg1 '#hash())
                                        (notf (predf 'C 'arg0 '#hash()))))) `arg0)
        (related-otherf
         (andf
          (orf (notf (predf 'Y 'arg0 '#hash())) (orf (notf (predf 'A 'arg0 '#hash())) (notf (predf 'B 'arg0 '#hash()))))
          (orf (notf (predf 'Y 'arg0 '#hash())) (iff (predf 'D 'arg0 '#hash()) (predf 'E 'arg0 '#hash()) (predf 'F 'arg0 '#hash()))))
         (andf
          (orf (predf 'Z 'arg1 '#hash()) (orf (notf (predf 'A 'arg0 '#hash())) (notf (predf 'B 'arg0 '#hash()))))
          (orf (predf 'Z 'arg1 '#hash()) (notf (predf 'C 'arg0 '#hash()))))))


  (test (cnf->relevant-fact (andf
                             (andf (orf (notf (predf 'Y 'arg0 '#hash()))
                                        (orf (notf (predf 'A 'arg0 '#hash()))
                                             (notf (predf 'B 'arg0 '#hash()))))
                                   (orf (predf 'Z 'arg1 '#hash())
                                        (orf (notf (predf 'A 'arg0 '#hash()))
                                             (notf (predf 'B 'arg0 '#hash())))))
                             (andf (orf
                                    (notf (predf 'Y 'arg0 '#hash()))
                                    (iff (predf 'D 'arg0 '#hash())
                                         (predf 'E 'arg0 '#hash())
                                         (predf 'F 'arg0 '#hash())))
                                   (orf (predf 'Z 'arg1 '#hash())
                                        (notf (predf 'C 'arg0 '#hash()))))) `arg1)
        (related-otherf
         (anyf)
         (andf
          (andf
           (orf (notf (predf 'Y 'arg0 '#hash())) (orf (notf (predf 'A 'arg0 '#hash())) (notf (predf 'B 'arg0 '#hash()))))
           (orf (predf 'Z 'arg1 '#hash()) (orf (notf (predf 'A 'arg0 '#hash())) (notf (predf 'B 'arg0 '#hash())))))
          (andf (orf (notf (predf 'Y 'arg0 '#hash())) (iff (predf 'D 'arg0 '#hash()) (predf 'E 'arg0 '#hash()) (predf 'F 'arg0 '#hash()))) (orf (predf 'Z 'arg1 '#hash()) (notf (predf 'C 'arg0 '#hash())))))))
  (test
   (cnf->relevant-fact (andf
                        (andf (orf (notf (predf 'Y 'arg0 '#hash()))
                                   (orf (notf (predf 'A 'arg0 '#hash()))
                                        (notf (predf 'B 'arg0 '#hash()))))
                              (orf (predf 'Z 'arg1 '#hash())
                                   (orf (notf (predf 'A 'arg0 '#hash()))
                                        (notf (predf 'B 'arg0 '#hash())))))
                        (andf (orf
                               (notf (predf 'Y 'arg0 '#hash()))
                               (notf (predf 'C 'arg0 '#hash())))
                              (orf (predf 'Z 'arg1 '#hash())
                                   (notf (predf 'C 'arg0 '#hash()))))) `arg1)
   (related-otherf (anyf) (andf
                 (andf (orf (notf (predf 'Y 'arg0 '#hash()))
                            (orf (notf (predf 'A 'arg0 '#hash()))
                                 (notf (predf 'B 'arg0 '#hash()))))
                       (orf (predf 'Z 'arg1 '#hash())
                            (orf (notf (predf 'A 'arg0 '#hash()))
                                 (notf (predf 'B 'arg0 '#hash())))))
                 (andf (orf
                        (notf (predf 'Y 'arg0 '#hash()))
                        (notf (predf 'C 'arg0 '#hash())))
                       (orf (predf 'Z 'arg1 '#hash())
                            (notf (predf 'C 'arg0 '#hash()))))) )))

(define/contract (related-fact fact arg)
  (-> Facts? any/c related-otherf?)
  ;(define sim-fact (simplify-facts fact))
  (define nnf (to-nnf fact))
  (define cnf (nnf->cnf nnf))
  (cnf->relevant-fact cnf arg))

(provide
 related-fact)

;(module+ test
;  (test (related-fact (notf (iff
;                             (iff (predf 'pred1? 'arg0 '#hash())
;                                  (predf 'pred1? 'arg0 '#hash())
;                                  (nonef))
;                             (iff (predf 'pred3? 'arg1 '#hash())
;                                  (nonef) (anyf)) (anyf))) `arg0)
;        (cons (predf 'pred1? 'arg0 '#hash()) (andf (predf 'pred1? 'arg1 '#hash()) (predf 'pred3? 'arg1 '#hash()))))

;  (test (related-fact (notf (iff
;                             (iff (predf 'pred1? 'arg0 '#hash())
;                                  (predf 'pred1? 'arg1 '#hash())
;                                  (nonef))
;                             (iff (predf 'pred3? 'arg1 '#hash())
;                                  (nonef) (anyf)) (anyf))) `arg1)
;        (cons (andf (predf 'pred1? 'arg1 '#hash()) (predf 'pred3? 'arg1 '#hash())) (predf 'pred1? 'arg0 '#hash()))))
