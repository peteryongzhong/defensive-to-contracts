#lang racket
(require (only-in plai define-type test print-only-errors))
(require "definitions.rkt")

(define (fact-equal? arg1 arg2)
  (cond
    [(and (orf? arg1)
          (orf? arg2)
          (or (equal? arg1 arg2)
              (equal? (orf (orf-i arg1) (orf-c arg1)) arg2)))]
    [(and (andf? arg1)
          (andf? arg2)
          (or (equal? arg1 arg2)
              (equal? (orf (andf-i arg1) (andf-c arg1)) arg2)))] 
    [(and (notf? arg1)
          (notf? arg2))
          (fact-equal? (notf-i arg1) (notf-i arg2))]
    [else (equal? arg1 arg2)]))

(match-equality-test fact-equal?)


(define/contract (simplify-facts fact)
  (Facts? . -> . Facts?)
  (define (basic-simplify fact)
    (match fact  
      [(andf inner (nonef)) (nonef)]
      [(andf inner (anyf)) (basic-simplify inner)]
      [(andf (nonef) inner) (nonef)]
      [(andf (anyf) inner) (basic-simplify inner)]
      [(andf inner inner) (basic-simplify inner)]
      [(andf fir sec) (andf (basic-simplify fir) (basic-simplify sec))]
      [(orf inner (nonef)) (basic-simplify inner)]
      [(orf inner (anyf)) (anyf)]
      [(orf (nonef) inner ) (basic-simplify inner)]
      [(orf (anyf) inner) (anyf)]
      [(orf inner inner) (basic-simplify inner)]
      [(orf fir sec) (orf (basic-simplify fir) (basic-simplify sec))]
      [(notf (iff tst thn els)) (iff (basic-simplify tst)
                                    (basic-simplify (notf thn))
                                    (basic-simplify (notf els)))]
      [(notf (notf inner)) (basic-simplify inner)]
      [(notf (anyf)) (nonef)]
      [(notf (nonef)) (anyf)]
      [(notf inner) (notf (basic-simplify inner))]
      [(iff (anyf) thn els) (basic-simplify thn)]
      [(iff (nonef) thn els) (basic-simplify els)]      
      ;[(iff tst (anyf) (anyf)) (anyf)]
      ;[(iff tst (nonef) (nonef)) (nonef)]
      [(iff tst cond cond) (basic-simplify cond)]
      [(iff tst (nonef) els) (andf (basic-simplify (notf tst)) (basic-simplify els))]
      [(iff tst thn (nonef)) (andf (basic-simplify tst) (basic-simplify thn))]
      [(iff tst (anyf) els) (orf (basic-simplify tst) (basic-simplify els))]
      [(iff tst thn (anyf)) (orf (basic-simplify (notf tst)) (basic-simplify thn))]
      [(iff tst thn els) (iff (basic-simplify tst) (basic-simplify thn) (basic-simplify els))]
      [_ fact]))

  (define (progressively-simplify-facts fact)
    (let ([new-fact (simplify-facts-inner (basic-simplify fact) '())])
      (if (equal? fact new-fact)
          fact
          (progressively-simplify-facts new-fact))))

  ; check if inner is redundent assuming known is known
  (define (check-inner-against inner known)
    (cond
      [(member inner known fact-equal?) (anyf)]
      [(and (notf? inner) (member (notf-i inner) known fact-equal?))
       (nonef)]
      [(member (notf inner) known fact-equal?)
       (nonef)]
      [else inner]))

  (define (simplify-facts-inner fact known)
    (match fact
      [(andf fir sec) (andf (simplify-facts-inner fir known) (simplify-facts-inner sec (cons fir known)))]
      [(orf fir sec) (orf (simplify-facts-inner fir known) (simplify-facts-inner sec (cons (notf fir) known)))]
      ;[(notf (notf cond)) (check-inner-against cond known)]
      [(notf cond) (notf (simplify-facts-inner cond known))]
      [(iff tst thn els) (iff (simplify-facts-inner tst known)
                              (simplify-facts-inner thn (cons tst known))
                              (simplify-facts-inner els (cons (notf tst) known)))]
      [_ (check-inner-against fact known)]))
  
  (progressively-simplify-facts fact))

(provide 
 simplify-facts)


(module+ test
  (print-only-errors)
  (test (simplify-facts (andf  (predf 'positive? 'arg0 '#hash()) (nonef)))
        (nonef))
  (test (simplify-facts (andf (anyf) (predf 'positive? 'arg0 '#hash())))
        (predf 'positive? 'arg0 '#hash()))
  (test (simplify-facts (orf (nonef)  (predf 'positive? 'arg0 '#hash())))
        (predf 'positive? 'arg0 '#hash()))
  (test (simplify-facts (orf (anyf)  (predf 'positive? 'arg0 '#hash())))
        (anyf))
  (test (simplify-facts (orf (predf 'positive? 'arg0 '#hash())  (predf 'positive? 'arg0 '#hash())))
        (predf 'positive? 'arg0 '#hash()))
  (test (simplify-facts (orf (notf (notf (anyf)))  (predf 'positive? 'arg0 '#hash())))
        (anyf))
  (test (simplify-facts (andf (notf (predf 'positive? 'arg0 '#hash())) (predf 'positive? 'arg0 '#hash())))
        (nonef))
  (test (simplify-facts (orf (predf 'positive? 'arg0 '#hash()) (notf (nonef))))
        (anyf))
  (test (simplify-facts (andf (predf 'positive? 'arg0 '#hash()) (notf (predf 'positive? 'arg0 '#hash()))))
        (nonef))
  (test (simplify-facts (orf (notf (anyf))  (predf 'positive? 'arg0 '#hash())))
        (predf 'positive? 'arg0 '#hash()))
  
  (test (simplify-facts (orf (andf (predf 'positive? 'arg0 '#hash())
                                   (predf 'positive? 'arg0 '#hash()))
                             (andf (notf (predf 'positive? 'arg0 '#hash()))
                                   (predf 'exact-integer? 'arg0 '#hash()))))
        (orf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash())))
  (test (simplify-facts (orf (predf 'positive? 'arg0 '#hash())
                             (andf (notf (predf 'positive? 'arg0 '#hash()))
                                   (predf 'exact-integer? 'arg0 '#hash()))))
        (orf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash())))
  (test (simplify-facts (andf
                         (andf (notf
                                (orf (andf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash()))
                                     (andf (notf (predf 'positive? 'arg0 '#hash())) (nonef))))
                               (anyf))
                         (anyf)))
        (notf (andf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash()))))
  (test (simplify-facts (andf
                         (andf (anyf)
                               (notf
                                (orf (andf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash()))
                                     (andf  (nonef) (notf (predf 'positive? 'arg0 '#hash()))))) )
                         (anyf)))
        (notf (andf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash()))))

  (test (simplify-facts (andf
                         (notf
                          (andf
                           (notf
                            (orf (andf (predf 'positive? 'arg0 '#hash())
                                       (predf 'positive? 'arg0 '#hash()))
                                 (andf (notf (predf 'positive? 'arg0 '#hash()))
                                       (predf 'exact-integer? 'arg0 '#hash()))))
                           (anyf))) (anyf)))
        (orf (predf 'positive? 'arg0 '#hash()) (predf 'exact-integer? 'arg0 '#hash())))

  (test (simplify-facts (iff (predf 'pred1? 'arg0 '#hash())
                             (andf (predf 'pred2? 'arg0 '#hash()) (anyf))
                             (andf (notf (predf 'pred3? 'arg0 '#hash())) (anyf))))
        (iff (predf 'pred1? 'arg0 '#hash()) (predf 'pred2? 'arg0 '#hash()) (notf (predf 'pred3? 'arg0 '#hash()))))

  (test (simplify-facts (iff (predf 'pred1? 'arg0 '#hash())
                             (predf 'pred1? 'arg0 '#hash())
                             (notf (predf 'pred1? 'arg0 '#hash()))))
        (anyf))

  (test (simplify-facts (iff (predf 'pred1? 'arg0 '#hash())
                             (predf 'pred1? 'arg0 '#hash())
                             (predf 'pred1? 'arg0 '#hash())))
        (predf 'pred1? 'arg0 '#hash()))
  
  (test (simplify-facts (andf (predf 'pred1? 'arg0 '#hash())
                              (iff (predf 'pred1? 'arg0 '#hash())
                                   (andf (predf 'pred2? 'arg0 '#hash()) (anyf))
                                   (andf (notf (predf 'pred3? 'arg0 '#hash())) (anyf)))))
        (andf (predf 'pred1? 'arg0 '#hash()) (predf 'pred2? 'arg0 '#hash())))

  (test (simplify-facts (orf (predf 'pred1? 'arg0 '#hash())
                             (iff (predf 'pred1? 'arg0 '#hash())
                                  (andf (predf 'pred2? 'arg0 '#hash()) (anyf))
                                  (andf (notf (predf 'pred3? 'arg0 '#hash())) (anyf)))))
        (orf (predf 'pred1? 'arg0 '#hash()) (notf (predf 'pred3? 'arg0 '#hash()))))

  (test (simplify-facts (andf (predf 'pred3? 'arg0 '#hash())
                              (iff (predf 'pred1? 'arg0 '#hash())
                                   (andf (predf 'pred2? 'arg0 '#hash()) (anyf))
                                   (predf 'pred3? 'arg0 '#hash()))))
        (andf (predf 'pred3? 'arg0 '#hash()) (orf (notf (predf 'pred1? 'arg0 '#hash())) (predf 'pred2? 'arg0 '#hash()))))


  (test (simplify-facts (iff (predf 'pred1? 'arg0 '#hash())
                             (andf (predf 'pred1? 'arg0 '#hash()) (anyf))
                             (andf (notf (predf 'pred3? 'arg0 '#hash())) (anyf))))
        (orf (predf 'pred1? 'arg0 '#hash()) (notf (predf 'pred3? 'arg0 '#hash()))))

  (test (simplify-facts (andf (notf (predf 'pred3? 'arg0 '#hash()))
                              (iff (predf 'pred1? 'arg0 '#hash())
                                   (andf (predf 'pred3? 'arg0 '#hash()) (anyf))
                                   (andf (predf 'pred3? 'arg0 '#hash()) (anyf)))))
        (nonef))

  (test (simplify-facts (andf (notf (predf 'pred2? 'arg0 '#hash()))
                              (iff (predf 'pred1? 'arg0 '#hash())
                                   (andf (predf 'pred2? 'arg0 '#hash()) (anyf))
                                   (andf (predf 'pred3? 'arg0 '#hash()) (anyf)))))
        (andf (notf (predf 'pred2? 'arg0 '#hash())) (andf (notf (predf 'pred1? 'arg0 '#hash())) (predf 'pred3? 'arg0 '#hash()) ) ))
  
  (test (simplify-facts (andf (notf (predf 'pred2? 'arg0 '#hash()))
                              (iff (predf 'pred1? 'arg0 '#hash())
                                   (andf (predf 'pred3? 'arg0 '#hash()) (anyf))
                                   (andf (predf 'pred2? 'arg0 '#hash()) (anyf)))))
        (andf (notf (predf 'pred2? 'arg0 '#hash())) (andf (predf 'pred1? 'arg0 '#hash()) (predf 'pred3? 'arg0 '#hash()) ) ))
  (test (simplify-facts 
         (andf (orf (predf 'number? 'arg1 (hash 'arg1 (func-arg) 'arg2 (func-arg)))
                    (predf 'number? 'arg2 (hash 'arg1 (func-arg) 'arg2 (func-arg))))
               (orf (predf 'number? 'arg2 (hash 'arg1 (func-arg) 'arg2 (func-arg)))
                    (predf 'number? 'arg1 (hash 'arg1 (func-arg) 'arg2 (func-arg))))))
         (orf (predf 'number? 'arg1 (hash 'arg1 (func-arg) 'arg2 (func-arg)))
                    (predf 'number? 'arg2 (hash 'arg1 (func-arg) 'arg2 (func-arg)))))
  )