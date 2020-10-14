#lang racket
(require (only-in plai define-type test print-only-errors))
(require "definitions.rkt")
(require "chspans.rkt")

(define (put-bindings-to-scope bindings scope)
  (if (empty? bindings)
      scope
      (let ([binding-pair (first bindings)])
        (put-bindings-to-scope (rest bindings) (hash-set scope (car binding-pair) (sub-defined (cdr binding-pair)))))))

(define/contract (complex-pred-to-facts pred scope)
  (BasicExpand? (hash/c symbol? VariableContent?) . -> . (or/c Facts? #f))
  (match pred
    [(num n chs) (anyf)]
    [(symbolB s chs) (anyf)]
    [(errorB chs) #f] ; try to create a fact for this 
    [(lambdaB args body chs) (anyf)]
    [(appNotB expr chs)
             (define result (complex-pred-to-facts expr scope))
             (if (not result)
                 #f
                 (notf result))]
    [(appB func-name expr-list chs)  (appB->facts pred scope)]
;    [appB (func-name expr-list chs) (if (= (length expr-list) 1)
;                                    (let ([inner-program (first expr-list)])
;                                      (if (and (idB? inner-program) (idB? func-name))
;                                          (let ([arg-name (idB-name inner-program)])
;                                            (if (and (func-arg? (hash-ref scope arg-name #f))
;                                                     (not (hash-ref scope (idB-name func-name) #f)))
;                                                (predf (idB-name func-name) arg-name (hash)) ;doens;t matter maybe just pritner
;                                                (predf (idB-name func-name) arg-name scope)))
;                                          (complexf pred scope)))
;                                    (complexf pred scope))] ; constant > arg 7
    [(bool v chs) (if v
                  (anyf)
                  (nonef))]
    [(idB name chs)
     (match (hash-ref scope name)
       [(func-arg) (notf (predf `false? name (hash)))]
       [(sub-defined def-expr)
        (define result (complex-pred-to-facts def-expr scope))
        (if (not result)
            #f
            result)])]
    [(ifB tst thn els chs)
         (define tst-fact (complex-pred-to-facts tst scope))
         (define thn-fact (complex-pred-to-facts thn scope))
         (define els-fact (complex-pred-to-facts els scope))
         (if (not (and tst-fact thn-fact els-fact))
             #f
             (iff tst-fact thn-fact els-fact))] 
    [(letB bindings body chs)
          (define result (complex-pred-to-facts body (put-bindings-to-scope bindings scope)))
          (if (not result)
              #f
              result)]))

(define (appB->facts app scope)
  (define (is-func-arg? arg)
    (if (idB? arg)
        (func-arg? (hash-ref scope (idB-name arg) #f))
        #f))
  (match  app
    [(appB func-name expr-list chs)
          (cond
            [(not (idB? func-name))
             (complexf app scope)]
            [(= (length expr-list) 1)
              (let ([inner-program (first expr-list)])
                (if (idB? inner-program)
                    (predf (idB-name func-name) (idB-name inner-program) scope)
                    (complexf app scope)))]
            [else
             (let
                 ([func-args (filter is-func-arg? expr-list)]
                  [constants  (filter constants? expr-list)])
               (if (and (= (length func-args) 1)
                        (= (+ (length constants) 1) (length expr-list)))
                   (complex-predf
                    (idB-name func-name)
                    (idB-name (first func-args))
                    scope
                    app)
                   (complexf app scope)))])]
    [_ (error "did not expect others")]))
  
(define (constants? program)
  (cond
    [(num? program) #t]
    [(symbolB? program) #t]
    [(bool? program) #t]
    [else #f]))

(provide 
 complex-pred-to-facts
 put-bindings-to-scope)

(module+ test
  (print-only-errors)
  (test (complex-pred-to-facts
         (num 5 (empty-charset))
         (hash `arg0 (func-arg)))
        (anyf)
        )
  (test (complex-pred-to-facts
         (ifB 
          (appB (idB `pred1 (empty-charset)) (list (idB `arg0 (empty-charset))) (empty-charset))
          (appB (idB `void (empty-charset)) (list ) (empty-charset))
          (errorB (empty-charset)) (empty-charset))
         (hash `arg0 (func-arg)))
        #f)
  (test (complex-pred-to-facts
         (ifB
          (appB (idB`pred1 (empty-charset)) (list (idB`arg0(empty-charset))) (empty-charset))
          (appB (idB `void (empty-charset)) (list ) (empty-charset))
          (ifB
           (appB (idB`pred2 (empty-charset)) (list (idB`arg0 (empty-charset))) (empty-charset))
           (appB (idB`void (empty-charset)) (list ) (empty-charset))
           (errorB(empty-charset))
           (empty-charset))
          (empty-charset))
         (hash `arg0 (func-arg)))
        #f)
  (test (complex-pred-to-facts
         (appB (idB`pred1 (empty-charset)) (list (idB`arg0 (empty-charset))) (empty-charset))
         (hash `arg0 (func-arg)))
        (predf 'pred1 'arg0 (hash 'arg0 (func-arg))))
  (test (complex-pred-to-facts
         (appNotB (appB (idB`pred1(empty-charset)) (list (idB`arg0(empty-charset))) (empty-charset)) (empty-charset))
         (hash `arg0 (func-arg)))
        (notf (predf 'pred1 'arg0(hash 'arg0 (func-arg)))))
  (test (complex-pred-to-facts
         (bool #t (empty-charset))
         (hash `arg0 (func-arg)))
        (anyf))
  (test (complex-pred-to-facts
         (bool #f (empty-charset))
         (hash `arg0 (func-arg)))
        (nonef))
  (test (complex-pred-to-facts  (letB (list (cons 'positive-odd?
                                                  (lambdaB '(n) (ifB (ifB (appB (idB'positive? (empty-charset)) (list (idB'n (empty-charset))) (empty-charset))
                                                                          (appB (idB'odd? (empty-charset))
                                                                                (list (idB'n (empty-charset))) (empty-charset))
                                                                          (bool #f (empty-charset))(empty-charset) )
                                                                     (bool #t (empty-charset)) (bool #f (empty-charset))
                                                                     (empty-charset)) (empty-charset))))
                                      (appB (idB'positive-odd? (empty-charset)) (list (idB'arg0 (empty-charset))) (empty-charset)) (empty-charset))
                                (hash `arg0 (func-arg)))
        (predf 'positive-odd? 'arg0
               (hash 'arg0 (func-arg) 'positive-odd?
                     (sub-defined (lambdaB '(n) (ifB (ifB (appB (idB'positive? (empty-charset)) (list (idB'n (empty-charset))) (empty-charset))
                                                                          (appB (idB'odd? (empty-charset))
                                                                                (list (idB'n (empty-charset))) (empty-charset))
                                                                          (bool #f (empty-charset))(empty-charset) )
                                                                     (bool #t (empty-charset)) (bool #f (empty-charset))
                                                                     (empty-charset)) (empty-charset))))))
  (test (complex-pred-to-facts  
         (appB (idB 'positive-odd? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset))
         (hash 'arg0 (func-arg) 'positive-odd?
               (sub-defined
                (lambdaB '(n) (ifB (ifB (appB (idB'positive? (empty-charset)) (list (idB'n (empty-charset))) (empty-charset))
                                                                          (appB (idB'odd? (empty-charset))
                                                                                (list (idB'n (empty-charset))) (empty-charset))
                                                                          (bool #f (empty-charset))(empty-charset) )
                                                                     (bool #t (empty-charset)) (bool #f (empty-charset))
                                                                     (empty-charset)) (empty-charset)))))
        (predf 'positive-odd? 'arg0
               (hash 'arg0 (func-arg) 'positive-odd?
                     (sub-defined (lambdaB '(n) (ifB (ifB (appB (idB'positive? (empty-charset)) (list (idB'n (empty-charset))) (empty-charset))
                                                                          (appB (idB'odd? (empty-charset))
                                                                                (list (idB'n (empty-charset))) (empty-charset))
                                                                          (bool #f (empty-charset))(empty-charset) )
                                                                     (bool #t (empty-charset)) (bool #f (empty-charset))
                                                                     (empty-charset)) (empty-charset))))))
  (test (complex-pred-to-facts
         (appB (idB `pred1 (empty-charset)) (list (idB `arg0 (empty-charset)) (idB `arg1 (empty-charset))) (empty-charset))
         (hash `arg0 (func-arg) `arg1 (func-arg)))
        (complexf (appB (idB `pred1 (empty-charset)) (list (idB `arg0 (empty-charset)) (idB `arg1 (empty-charset))) (empty-charset)) (hash 'arg0 (func-arg) 'arg1 (func-arg))))
 
 (test (complex-pred-to-facts
         (ifB
          (appB (idB 'positive? (empty-charset)) (list (idB 'arg0 (empty-charset)))(empty-charset))
          (appB (idB 'exact-integer? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset))
          (bool #f (empty-charset))(empty-charset))
         (hash 'arg0 (func-arg) 'arg1 (func-arg)))
        (iff (predf 'positive? 'arg0 (hash 'arg0 (func-arg) 'arg1 (func-arg)))
             (predf 'exact-integer? 'arg0 (hash 'arg0 (func-arg) 'arg1 (func-arg)))
             (nonef)))
  (test
   (complex-pred-to-facts
    (letB (list (cons 'or-part (appB (idB 'positive? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset))))
          (ifB (idB 'or-part (empty-charset))
               (idB 'or-part (empty-charset))
               (appB (idB 'exact-integer? (empty-charset)) (list (idB 'arg0 (empty-charset)))(empty-charset))
               (empty-charset))
          (empty-charset))
    (hash 'arg0 (func-arg)))
   (iff
    (predf 'positive? 'arg0 (hash 'arg0 (func-arg) 'or-part (sub-defined (appB (idB 'positive? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset)))))
    (predf 'positive? 'arg0 (hash 'arg0 (func-arg) 'or-part (sub-defined (appB (idB 'positive? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset)))))
    (predf 'exact-integer? 'arg0 (hash 'arg0 (func-arg) 'or-part (sub-defined (appB (idB 'positive? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset)))))))


  (test (complex-pred-to-facts
         (appB (idB `> (empty-charset)) (list (idB`arg0 (empty-charset)) (num 1 (empty-charset))) (empty-charset))
         (hash `arg0 (func-arg)))
        (complex-predf '> 'arg0 (hash 'arg0 (func-arg)) (appB (idB '> (empty-charset)) (list (idB 'arg0 (empty-charset)) (num 1 (empty-charset))) (empty-charset))))
  (test (complex-pred-to-facts (idB `arg0 (empty-charset))
                               (hash 'arg0 (func-arg) 'arg1 (func-arg))) 
        (notf (predf 'false? 'arg0 '#hash()))))