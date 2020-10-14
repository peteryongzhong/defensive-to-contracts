#lang racket

(require "definitions.rkt")
(require "program-fact.rkt")
(require "simplify-fact.rkt")
(require "related-fact.rkt")
(require "fact-to-contract.rkt")
(require "structure-deletions.rkt")
(require syntax/parse)
(require "chspans.rkt")
(require syntax/modread)
;(require "textedit.rkt")
(require "pcond.rkt")

; c for the contract statement, f for unused facts
(struct contracts&unused (c f)
  #:guard (struct-guard/c any/c Facts?)) 

(struct fact-and-removals (f r)
  #:guard (struct-guard/c Facts? character-set?))



;(define-type FactAndRemovals
;  [fact-and-removals (f Facts?) ;fact
;                     (r character-set?)])

(define (extract-program pth exp?)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-directory (path-only pth)]
                 [current-load-relative-directory (path-only pth)])
    (define exp
      (call-with-input-file pth
        (λ (port)
          (port-count-lines! port)
          (with-module-reading-parameterization
            (λ ()
              (read-syntax pth port))))))
    (if exp? (expand exp) exp)))

(define (process-sub-list subexps acc descension-func)
  (if (equal? subexps '())
      acc
      (process-sub-list (rest subexps) (append acc (descension-func (first subexps))) descension-func)))

(define (get-expanded-func-syn path)
  (define (findexpand-func-body-in-original syn)
    (syntax-parse syn
      [((~datum define) args body) (list syn)]
      [(syms ...+) (process-sub-list (syntax-e #'(syms ...)) '() findexpand-func-body-in-original) ]
      [_ '()]))
  (findexpand-func-body-in-original path))


(define (path-addcontract path [debug #f])
  ; go by source location ordering, sort them based on that
  ;visually indicate group
  ; left and right enable -> change.
  ; function bascially apply, but in a new window, call that function whenever things change.
  ;use function name/source location instead
  ;enable line number on the text% obj
  ;begin-edit-seqence, end. 

  (define (findfunc-body-in-original syn)
    (syntax-parse syn
      [((~datum define) (args ...) body ...) (list syn)]
      [((~datum define) arg body) '()]
      [(syms ...) (process-sub-list (syntax-e #'(syms ...)) '() findfunc-body-in-original)]
      [_ '()]))

  (define (find-expand-func-body syn)
    (syntax-parse syn
      [((~datum define-values) (_) ((~datum lambda) _ _ ... )) (list syn)]
      [((~datum define-values) (name) (let-values (((name1) ((~datum lambda) _ bodies ... ))) _)) (if (equal? (syntax-e #'name)
                                                                                                              (syntax-e #'name1))
                                                                                                      (list syn)
                                                                                                      '())]
      [(syms ...) (process-sub-list (syntax-e #'(syms ...)) '() find-expand-func-body) ]
      [_ '()]))
  
  (define func-bodies-rev (reverse (findfunc-body-in-original (extract-program path #f))))
  (define expanded-body (extract-program path #t))
  (when debug
    (print expanded-body))
  (define expanded-func-bodies-rev (reverse (find-expand-func-body expanded-body)))
  (when debug
    (print func-bodies-rev))
  (define error-list '())
  
  (define contract-infos (filter (lambda (x) (if x #t #f))
                                 (for/list ([func func-bodies-rev]
                                            [expanded expanded-func-bodies-rev])
                                   (with-handlers (
                                                   [exn:fail?
                                                    (λ (e) (set! error-list (cons (exn-message e) error-list)) #f)])
                                     (func->contractinfo func expanded path debug)))))
  (contract-infos&errors contract-infos error-list))



(define (func->contractinfo original-syn expanded-syn path [debug #f])
  ;(define original-syn (first (findexpand-func-body-in-original (extract-program path #f))))
  (when debug
    (print original-syn))

  (define-values (def-syn func-name listargs syn-bodies-unexpanded)
    (syntax-parse original-syn
      [((~and (~datum define) def) (name args ...) body ...)
       (values
        #'def
        (syntax->datum #'name)
        (syntax->datum #'(args ...)) 
        (syntax-e #'(body ...)))]))
  (when (not (equal? (length (filter symbol? listargs))
                     (length listargs)))
    (error "optional parameter detected"))
  (define syn-bodies
    (syntax-parse expanded-syn
      [((~datum define-values) (_) ((~datum lambda) _ bodies ... ))
       (syntax-e #'(bodies ...))]
      [((~datum define-values) (name) (let-values (((name1) ((~datum lambda) _ bodies ... ))) _)) 
       (syntax-e #'(bodies ...))]))
  
  (define (process-bodies bodies [acc #f])
    (if (empty? bodies)
        acc
        (let ([fact&deletions (syntax->contract (first bodies) listargs path debug)])
          (if (not acc)
              (process-bodies (rest bodies) fact&deletions)
              (process-bodies (rest bodies) (fact-and-removals
                                             (simplify-facts (andf (fact-and-removals-f fact&deletions)
                                                                   (fact-and-removals-f acc)))
                                             (union-charset (fact-and-removals-r fact&deletions)
                                                            (fact-and-removals-r acc))))))))
  (define facts&removals (process-bodies syn-bodies)) 

  (define (contract-on-arguments args fact [contracts-acc '()])
    (if (empty? args)
        (contracts&unused contracts-acc fact)
        (let ([related-relevant-unused-fact (related-fact fact (first args))])
          (let ([relevant-fact (related-otherf-r related-relevant-unused-fact)] 
                [unused-fact (related-otherf-o related-relevant-unused-fact)])
            (contract-on-arguments (rest args) unused-fact
                                   (cons (fact->contract-simplified-flattened relevant-fact (first args)) contracts-acc))))))
  (define contracts-and-unused (contract-on-arguments listargs (fact-and-removals-f facts&removals)))
  (define residual-facts (simplify-facts (contracts&unused-f contracts-and-unused)))
  (when  debug
    (printf "\n\nresidual facts\n")
    (print residual-facts)
    (printf "\n\n#pre statements\n"))
  (define hpre-statement (fact->#pre residual-facts))
  (when debug
    (print hpre-statement)
    (printf "\n\nraw contract\n")
    (print (reverse (contracts&unused-c contracts-and-unused))))
  (define desirability 10)
  (set! desirability  (- desirability (truncate (* (/ (length (filter (lambda (c) (or (equal? c 'any)
                                                                                      (equal? c 'any/c))) (contracts&unused-c contracts-and-unused)))
                                                      (length (contracts&unused-c contracts-and-unused)))
                                                   5))))
  (define removals (fact-and-removals-r facts&removals))
  (define removal-length (charset-size removals))
  ;(print "removal-length\n")
  ;(print removal-length)
  (cond
    [(< removal-length 5) (set! desirability (- desirability 3))]
    [(< removal-length 50) (set! desirability (- desirability 2))]
    [(< removal-length 100) (set! desirability (- desirability 1))])
  (when (not (equal? hpre-statement '#t))
    (set! desirability (+ desirability 5)))
      
  (define (dependent-contract-generate contracts hpre-statement)
    `(->i
      ,(for/list ([arg listargs]
                  [contract contracts])
         `[,arg ,contract])
      #:pre ,listargs
      ,hpre-statement
      any))
  
  (define contract-statement
    (if (equal? hpre-statement '#t)
        (cons `-> (append (reverse (contracts&unused-c contracts-and-unused)) (list `any)))
        (dependent-contract-generate (reverse (contracts&unused-c contracts-and-unused)) hpre-statement)))
  ;(define contract-statement `(->* ,(reverse (contracts&unused-c contracts-and-unused)) #:pre ,hpre-statement any))
  ;(print  (dependent-contract-generate (reverse (contracts&unused-c contracts-and-unused)) hpre-statement))
  (when debug
    (printf "\n\ncontract statement\n")
    (print contract-statement))
  (func-contract-info
   func-name
   path
   removals
   contract-statement
   (+ (syntax-position def-syn) (syntax-span def-syn))
   (syntax-position (first syn-bodies-unexpanded))
   desirability))

(define (syntax->contract expanded-body listargs path [debug #f])
  ;  (define expanded-body (parameterize ([current-namespace
  ;                                        (make-base-namespace)])
  ;                          (namespace-require '"pcond.rkt")
  ;                          (expand syn-body))) ; expand whole file inspect modules, skip over modules top level consider those with the same source location
  ;use chekc boxes to allow everything displayed at once.
  (when debug
    (printf "expanded body: \n")
    (print expanded-body))
  (define program (parse-syn path expanded-body ))
  (when debug
    (printf "\n\nparsed body\n")
    (print program))
  (define context (make-immutable-hash (for/list ([arg listargs])
                                         (cons arg (func-arg)))))

  (when debug
    (printf "\n\ncontext:\n")
    (print context))
  (define program-structure (program-to-structure program context))
  (when debug
    (printf "\n\nprogram structure:\n")
    (print program-structure))
  (define char-operations (structure-deletions program-structure))
  (define program-fact (structure-to-fact program-structure))
  (when debug
    (printf "\n\nfact prior to simplifcation\n")
    (print program-fact))
  (define simplified-fact (simplify-facts program-fact))
  (when debug
    (printf "\n\nfact simplified:\n")
    (print simplified-fact)
    (printf "\n\n"))
  (fact-and-removals simplified-fact
                     char-operations)
  )
(provide path-addcontract)

;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/simple-defensive-two-arg.rkt");
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/double-errors-nested-ifs.rkt")
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/unless-one-arg.rkt")
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/contractable-two-args.rkt")
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/realworld2.rkt")
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/realworld3.rkt")
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/racketcon.rkt");
;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/three-funcs.rkt")
;(path-addcontract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/pcond-onearg.rkt")
;(path-addcontract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/pcond-entire.rkt")
;(path-addcontract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/pcond-twobranches.rkt")

;(path-addcontract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/simple-dependent.rkt")

;(path-addcontract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/contract-simplifcation.rkt")
;(path-addcontract "/Users/peterzhong/Library/Racket/7.8/pkgs/2048/gui/preferences-redirect.rkt")
;contract-stronger? used to check contract. flatten the and/c and use contract-stronger. 



; abstraction on pred only on arg (e.g. (pred constant arg constant ...)), and a further abstraction can be taken place, generalise to expression with X, with lambda fall back
;(done)

; result any not any/c
;(done)

; contract processing
;(done)

; protocal for expanding cond? and other macros.
; cond expand to cond but drops in information? Pcond lol.

; potentially simplify contracts, looking through to 

;(path-addcontract  "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/realworld2.rkt")
;(path-addcontract   "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/cond-one-arg.rkt")
; beautiful racket contract


;(syntax->contract syn-simple-defensive-two-arg)
; 
;(syntax->contract syn-contractable-on-two-args)
;
;(syntax->contract syn-cond-double-errors)
;
;(syntax->contract syn-double-error-nested-ifs)
;
;(syntax->contract syn-cond-one-args)
;
;(syntax->contract syn-simple-defensive-with-or)

;check if everything's
; port-count-lines! open file
; text:basic<%>, create this object?, use text:basic%
; character is between indicies. make sure clear about indicies.
; provide function abstractions, to convert bet
;
;(test
; (charset=? 
;  (syntax->contract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/simple-defensive-two-arg.rkt")
;  (charset-fromsets (list (charset 64 75) (charset 143 1))))
; #t)
;
;(test
; (charset=? 
;  (syntax->contract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/double-errors-nested-ifs.rkt")
;  (charset-fromsets (list (charset 87 56) (charset 147 1) (charset 159 33) (charset 196 26))))
; #t)
;
;(test
; (charset=? 
;  (syntax->contract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/unless-one-arg.rkt") 
;  (charset-fromsets (list (charset 47 55))))
; #t)
;(test
; (syntax->contract "/Users/peterzhong/Desktop/OneDrive/Northwestern/Research/defensive-anlysis-removal/sample-defensive/cond-one-arg.rkt")
; (character-set (set)))


;(define (pred1? arg)
;  #t)
;
;(define (pred2? arg)
;  #t)
;
;(define (pred3? arg)
;  #t)
;
;
;(define syn-simple-defensive-two-arg
;  #'(define (simple-defensive-two-arg arg0 arg1)
;      (if (not (real? arg0))
;          (error "arg0 needs to be real")
;          arg0)))
;
;(define syn-simple-defensive-with-or
;  #'(define (simple-defensive-with-or arg0 arg1)
;      (if (or (real? arg0) (positive? arg0))
;          (error "arg0 needs to be real")
;          arg0)))
; 
;(define syn-contractable-on-two-args
;  #'(define (contractable-on-two-args arg0 arg1)
;      (if (and (pred1? arg0) (pred1? arg1))
;          (if (pred3? arg1)
;              (void)
;              (error "pred 3 must be true for arg1"))
;          (error "pred1 is on both arguments"))))
;
;(define syn-cond-one-args
;  #'(define (cond-one-args arg0)
;      (cond
;        [(pred1? arg0) arg0]
;        [(not (pred2? arg0)) arg0]
;        [(not (pred3? arg0)) arg0]
;        [else (error "requirements need ot be filled")])))
;
;(define syn-double-error-nested-ifs
;  #'(define (contractable-on-two-args arg0)
;      (if (pred1? arg0)
;          (if (pred2? arg0)
;              (error "")
;              arg0)
;          (if (pred3? arg0)
;              arg0
;              (error "")))))
;
;(define syn-cond-double-errors
;  #'(define (cond-double-errors arg0)
;      (cond
;        [(pred2? arg0) (error "can't be pred2")]
;        [(pred1? arg0) arg0]
;        [(not (pred3? arg0)) arg0]
;        [else (error "requirements need ot be filled")])))
;
;(define syn-nested-double-errors
;  #'(define (cond-double-errors arg0)
;      (cond
;        [(pred2? arg0) (error "can't be pred2")]
;        [(pred1? arg0) arg0]
;        [(not (pred3? arg0)) arg0]
;        [else (error "requirements need ot be filled")])))


  


;  (define body (first
;                (findexpand-func-body-in-original
;                 (extract-program path #f))))
;  (parameterize ([current-namespace (make-base-namespace)])
;                          (expand body)))



;(define (path-addcontract path)
;  (define (findexpand-func-body-in-original syn)
;    (syntax-parse syn
;      [((~datum define) args body ...) (list syn)]
;      [(syms ...+) (process-sub-list (syntax-e #'(syms ...)) '() findexpand-func-body-in-original) ]
;      [_ '()]))
;  (define functions (reverse (findexpand-func-body-in-original (extract-program path #f))))
;  (print functions)
;  
;  (define contract-infos (for/list ([func functions])
;                           (func->contractinfo func path)))
;  (process-file-with-func-contractinfos contract-infos path))