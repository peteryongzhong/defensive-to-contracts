#lang racket
(require (only-in plai define-type test test/exn print-only-errors))
(require "definitions.rkt")
(require "chspans.rkt")

(define (fact->contract fact argument)
  (match fact
    [(anyf) `any/c]
    [(nonef) `none/c]
    [(andf l r) `(and/c ,(fact->contract l argument) ,(fact->contract r argument))]
    [(orf l r) `(or/c ,(fact->contract l argument) ,(fact->contract r argument))]
    [(notf i) `(not/c ,(fact->contract i argument))] 
    [(predf pred arg scope) #:when (equal? arg argument)  pred]
    [(predf pred _ scope)
     (error "Current version only supports predicates on the argument supplied")]
    [(complex-predf _ arg _ _) #:when (equal? arg argument)  (complex-predf->contract fact)]
    [(complexf program scope)
     (error "Current version only supports simple predicates")]
    [(iff tst thn els)
     `(if/c ,(fact->contract tst argument) ,(fact->contract thn argument) ,(fact->contract els argument))]
    [_ `any]))

(define (process-subunits-for-and subunit)
  (match subunit
    [`(and/c ,preds ...)
     preds]
    [`(or/c ,_ ...)
     (list (flatten-contract-inner subunit))]
    [_ (list subunit)]))

(define (process-subunits-for-or subunit)
  (match subunit
    [`(or/c ,preds ...)
     preds]
    [`(and/c ,_ ...)
     (list (flatten-contract-inner subunit))]
    [_ (list subunit)]))

(define (flatten-once lst)
  (apply append
         (map (lambda (e) (if (cons? e) e (list e)))
              lst)))

(define (flatten-contract-inner contract)
  (match contract
    [`(or/c ,pred)
     pred]
    [`(and/c ,pred)
     pred]
    [`(and/c ,preds ...)
     (flatten-once
        `(and/c
         ,(flatten-once
          (for/list
              ([pred preds])
            (process-subunits-for-and pred)))))]
    [`(or/c ,preds ...)
     (flatten-once
        `(or/c
         ,(flatten-once
          (for/list
              ([pred preds])
            (process-subunits-for-or pred)))))]
    [_ contract]))

(define (flatten-contract contract)
  (let ([new-contract (flatten-contract-inner contract)])
      (if (equal? contract new-contract)
          contract
          (flatten-contract new-contract))))

(define (contract-stronger-custom? left right)
  (cond
    [(contract-stronger? left right)
     #t]
    ;[(contract-equivalent? left right)
    ; #t] (see if counter?)
    [(and (equal? left exact-integer?)
          (equal? right real?))
     #t] ;add more here 
    [else #f]))

;(define (handshake ls)
;  (flatten-once
;  (for/list ([i (in-range (- (length ls) 1))])
;     (for/list ([j (in-range (+ i 1) (length ls))])
;        (list
;         (list-ref ls i)
;         (list-ref ls j))))))

(define-namespace-anchor anc)

(define my-eval
  (let ((ns (namespace-anchor->namespace anc)))
    (lambda (expr)
      (with-handlers ([exn:fail? (lambda (v) #f)])
        (eval expr ns)))))

;list of, cons, list? 
(define (pairwise-simplify contract)
  (match contract
    [`(and/c ,preds ...)
     (define removals
       (filter (lambda(v)v)
               (flatten
                (for/list ([i (in-range (- (length preds) 1))])
                  (for/list ([j (in-range (+ i 1) (length preds))])
                    (define left (list-ref preds i))
                    (define right (list-ref preds j))
                    (define left-value (my-eval left))
                    (define right-value (my-eval right))
                    (if (and left-value right-value)
                        (cond
                          [(contract-stronger-custom? left-value right-value)
                           j]
                          [(contract-stronger-custom? right-value left-value)
                           i]
                          [else #f])
                        #f)
                    )))))
     (cons `and/c
           (for/list ([i (length preds)]
                      [elem preds]
                      #:when (not (member i removals)))
             elem))]
    [`(or/c ,preds ...)
     (define removals
       (filter (lambda(v)v)
               (flatten
                (for/list ([i (in-range (- (length preds) 1))])
                  (for/list ([j (in-range (+ i 1) (length preds))])
                    (define left (list-ref preds i))
                    (define right (list-ref preds j))
                     (define left-value (my-eval left))
                    (define right-value (my-eval right))
                    (if (and left-value right-value)
                        (cond
                          [(contract-stronger-custom? left-value right-value)
                           i]
                          [(contract-stronger-custom? right-value left-value)
                           j]
                          [else #f])
                        #f)
                    )))))
     (cons `or/c
           (for/list ([i (length preds)]
                      [elem preds]
                      #:when (not (member i removals)))
             elem))]
    [_ contract]))
      
(define (simplify-contract-inner contract)
  (match contract
    [`(and/c ,preds ...)
     (pairwise-simplify (cons `and/c (map simplify-contract-inner preds)))]
    [`(or/c ,preds ...)
     (pairwise-simplify (cons `or/c (map simplify-contract-inner preds)))]
    [_ contract]))
     
(define (simplify-flatten-contract contract)
  (let ([new-contract (flatten-contract-inner (simplify-contract-inner contract))])
      (if (equal? contract new-contract)
          contract
          (simplify-flatten-contract new-contract))))

(define (program-recovery an-basicexpand)
  (match an-basicexpand
    [(ifB tst thn els cs) `(if ,(program-recovery tst)
                            ,(program-recovery thn)
                            ,(program-recovery els))]
    [(num n cs) `,n]
    [(appNotB expr cs) `(not ,(program-recovery expr))]
    [(appB func expr-list cs) `,(append (list (program-recovery func))
                                        (map program-recovery expr-list))]
    [(idB name cs) `,name]
    [(bool v cs) `,v]
    [(letB bindings body cs) (void)]
    [(lambdaB args body cs) `(lambda ,args ,(program-recovery body))]))

(define (fact->#pre fact)
  (match fact
    [(andf l r) `(and ,(fact->#pre l) ,(fact->#pre r))]
    [(orf l r) `(or ,(fact->#pre l) ,(fact->#pre r))]
    [(anyf) `#t]
    [(nonef) `#f]
    [(notf f) `(not ,(fact->#pre f))]
    [(iff tst thn els) `(if ,(fact->#pre tst)
                           ,(fact->#pre thn)
                           ,(fact->#pre els))]
    
    [(predf pred name scope) `(,pred ,name)]
    [(complex-predf _ _ _ appB) (program-recovery appB)]
    [(complexf program scope) (program-recovery program)]))

(define (complex-predf->contract pred)
  (match pred
    [(complex-predf name arg scope innerApp)
                   (match innerApp
                     [(appB (idB '> _) (list (idB argname _) (num a-num _)) _)
                      `(>/c ,a-num)]
                     [(appB (idB '> _) (list (num a-num _) (idB argname _) ) _)
                      `(</c ,a-num)]
                     [(appB (idB '< _) (list (idB argname _) (num a-num _) ) _)
                      `(</c ,a-num)]
                     [(appB (idB '< _) (list (num a-num _) (idB argname _) ) _)
                      `(>/c ,a-num)]
                     [(appB (idB '< _) (list (num a-num1 _) (idB argname _) (num a-num2 _)) _)
                      `(and/c (>/c ,a-num1)
                              (</c ,a-num2))]
                     [(appB (idB '<= _) (list (num a-num1 _) (idB argname _) (num a-num2 _)) _)
                      `(between/c ,a-num1 ,a-num2)]
                     [_
                      `(lambda (,arg)
                         ,(program-recovery innerApp)
                         )]
                     )]
    [_ (error)]))

(define (fact->contract-simplified-flattened fact argument)
  (simplify-flatten-contract (fact->contract fact argument)))

(provide 
 fact->contract
 fact->#pre
 fact->contract-simplified-flattened)
(module+ test
  (print-only-errors)
  (test (flatten-contract `(or/c pred1? (or/c pred2? (and/c pred4? (and/c pred5? pred6?)))))
        '(or/c pred1? pred2? (and/c pred4? pred5? pred6?)))

  (test (flatten-contract `(and/c pred1? (or/c pred2? (or/c pred4? (and/c pred5? pred6?)))))
        '(and/c pred1? (or/c pred2? pred4? (and/c pred5? pred6?))))

  (test (flatten-contract `(and/c pred1? (or/c pred2? (or/c pred4? (and/c pred5? pred6? pred7? (and/c pred8? pred9?))))))
        '(and/c pred1? (or/c pred2? pred4? (and/c pred5? pred6? pred7? pred8? pred9?))))

  (test (flatten-contract `positive?)
        `positive?)

  (test (simplify-flatten-contract `(and/c exact-integer? real?))
        'exact-integer?)

  (test (simplify-flatten-contract `(and/c real? exact-integer?))
        'exact-integer?)

  (test (simplify-flatten-contract `(or/c real? exact-integer?))
        `real?)

  (test (simplify-flatten-contract `(or/c exact-integer? real? ))
        `real?)

  (test (simplify-flatten-contract  `(or/c (between/c 25 75) (between/c 0 100) (and/c exact-integer? real? )))
        `(or/c (between/c 0 100) exact-integer?))

    (test (simplify-flatten-contract  `(and/c (between/c 25 75) (between/c 0 100) (or/c exact-integer? real? fake?)))
          `(and/c (between/c 25 75) (or/c real? fake?)))

  (test (fact->contract (andf (orf (predf 'Y? 'arg0 '#hash()) (predf 'B? 'arg0 '#hash()))
                              (orf (predf 'Z? 'arg0 '#hash()) (predf 'B? 'arg0 '#hash()))) `arg0)
        '(and/c (or/c Y? B?) (or/c Z? B?)))

  (test (fact->contract (andf (andf (orf (notf (predf 'Y? 'arg0 '#hash()))
                                         (orf (notf (predf 'A? 'arg0 '#hash()))
                                              (notf (predf 'B? 'arg0 '#hash())))) 
                                    (anyf))
                              (orf (notf (predf 'Y? 'arg0 '#hash()))
                                   (notf (predf 'C? 'arg0 '#hash())))) `arg0)
        '(and/c (and/c (or/c (not/c Y?)
                             (or/c (not/c A?) (not/c B?))) any)
                (or/c (not/c Y?) (not/c C?))))

  (test (fact->contract (orf (notf (predf 'Y? 'arg0 '#hash()))
                             (orf (notf (predf 'A? 'arg0 '#hash()))
                                  (orf (notf (predf 'B? 'arg0 '#hash()))
                                       (nonef)))) `arg0)
        '(or/c
          (not/c Y?)
          (or/c
           (not/c A?)
           (or/c (not/c B?) none/c))))
;  (test/exn (fact->contract (orf (notf (predf 'Y? 'arg1 '#hash()))
;                                 (orf (notf (predf 'A? 'arg0 '#hash()))
;                                      (orf (notf (predf 'B? 'arg0 '#hash()))
;                                           (nonef)))) `arg0)
;            "Current version only supports predicates on the argument supplied")
  (test (fact->contract (notf (iff (predf 'pred1? 'arg0 '#hash()) (predf 'pred2? 'arg0 '#hash()) (notf (predf 'pred3? 'arg0 '#hash())))) `arg0)
        '(not/c (if/c pred1? pred2? (not/c pred3?))))
  ) 