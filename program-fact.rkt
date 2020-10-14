#lang racket
(require (only-in plai define-type test print-only-errors))
(require "definitions.rkt")
(require "chspans.rkt")
(require "complex-pred-to-facts.rkt")

(define (program-to-structure program scope [outer-let-chs #f])
  (define (get-proper-range original-chs)
    (if outer-let-chs
        outer-let-chs
        original-chs))
  (match program
    [(ifB tst thn els chs)
         (define tst-result (complex-pred-to-facts tst scope))
         (if tst-result
             (let ([thn-result (program-to-structure thn scope)]
                   [els-result (program-to-structure els scope)])
               (ifP tst-result thn-result els-result (get-proper-range chs)))
             (program-to-structure tst scope))]
    [(errorB chs)
            (errorP (get-proper-range chs))]
    [(letB bindings body chs)
     (if (charset-empty? chs)
         (program-to-structure body (put-bindings-to-scope bindings scope))
         (program-to-structure body (put-bindings-to-scope bindings scope) (get-proper-range chs)))]
    [_ (codeP (get-proper-range (BasicExpand-cs program)))])) 


(define (structure-to-fact structure)
  (define (structure-to-fact-inner structure)
    (match structure
      [(codeP cs) (nonef)]
      [(errorP cs) (anyf)]
      [(ifP tst thn els cs) (iff
                             tst
                             (structure-to-fact-inner thn)
                             (structure-to-fact-inner els))]))
  (notf (structure-to-fact-inner structure)))

(provide 
 structure-to-fact
 program-to-structure)


(module+ test
  (print-only-errors)
  (test (program-to-structure (ifB (appB (idB `pred1 (empty-charset)) (list (idB `arg0 (empty-charset))) (empty-charset))
                                   (appB (idB `void (empty-charset)) (list ) (empty-charset)) 
                                   (letB (list )
                                         (errorB (empty-charset))
                                         (empty-charset))
                                   (empty-charset))
                              (hash `arg0 (func-arg)))
         (ifP (predf 'pred1 'arg0 (hash 'arg0 (func-arg))) (codeP (empty-charset)) (errorP (empty-charset)) (empty-charset)))
  (test (program-to-structure (ifB
                               (ifB
                                (appB (idB 'positive? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset))
                                (appB (idB 'exact-integer? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset))
                                (bool #f (empty-charset))
                                (empty-charset))
                               (appB (idB 'void (empty-charset)) '() (empty-charset))
                               (letB '() (errorB (empty-charset)) (empty-charset))
                               (empty-charset))
                              (hash 'arg0 (func-arg)))
        (ifP
         (iff (predf 'positive? 'arg0 (hash 'arg0 (func-arg))) (predf 'exact-integer? 'arg0 (hash 'arg0 (func-arg))) (nonef))
         (codeP (empty-charset))
         (errorP (empty-charset))
         (empty-charset)))
  (test (program-to-structure (ifB
                               (ifB (appB (idB 'exact? (empty-charset)) (list (idB 'arg0 (empty-charset))) (empty-charset))
                                    (letB '() (errorB (empty-charset)) (empty-charset))
                                    (appB (idB 'void (empty-charset)) '() (empty-charset))
                                   (empty-charset))
                               (letB '() (errorB (empty-charset)) (empty-charset))
                               (appB (idB 'void (empty-charset)) '() (empty-charset))
                               (empty-charset) )
                              (hash 'arg0 (func-arg)))
        (ifP (predf 'exact? 'arg0 (hash 'arg0 (func-arg))) (errorP (empty-charset)) (codeP (empty-charset)) (empty-charset)))

  (test (structure-to-fact
         (ifP (predf 'exact? 'arg0 '#hash()) (errorP (empty-charset)) (codeP (empty-charset)) (empty-charset)))
        (notf (iff (predf 'exact? 'arg0 '#hash()) (anyf) (nonef))))

  
  )