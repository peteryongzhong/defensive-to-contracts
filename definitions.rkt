#lang plai
;(require (only-in plai define-type ))
(require syntax/parse)
(require racket/serialize)
(require "chspans.rkt")

(define-type BasicExpand 
  [ifB (tst BasicExpand?) 
       (thn BasicExpand?)
       (els BasicExpand?)
       (cs character-set?)]
  [num (n number?)
       (cs character-set?)]
  [symbolB (s any/c)
          (cs character-set?)]
  [appNotB (expr BasicExpand?)
           (cs character-set?)]
  [appB (func-name BasicExpand?)
        (expr-list (listof BasicExpand?))
        (cs character-set?)]
  [errorB  (cs character-set?)]
  [idB (name symbol?)
       (cs character-set?)]
  [bool (v boolean?)
        (cs character-set?)]
  [letB (bindings (listof (cons/c symbol? BasicExpand?)))
        (body BasicExpand?)
        (cs character-set?)]
  [lambdaB (args (listof symbol?))
           (body BasicExpand?)
           (cs character-set?)])

(define (BasicExpand-cs program)
  (cond
    [(ifB? program) (ifB-cs program)]
    [(num? program) (num-cs program)]
    [(appNotB? program) (appNotB-cs program)]
    [(appB? program) (appB-cs program)]
    [(errorB? program) (errorB-cs program)]
    [(idB? program) (idB-cs program)]
    [(bool? program) (bool-cs program)]
    [(letB? program) (letB-cs program)]
    [(lambdaB? program) (lambdaB-cs program)]))

;(struct BasicExpand
;  (cs))
;(struct ifB BasicExpand
;  (tst thn els))
;(struct num BasicExpand
;  (n))
;(struct symbolB BasicExpand
;  (s))
;(struct appNotB BasicExpand
;  (expr))
;(struct appB BasicExpand
;  (func-name expr-list))
;(struct errorB BasicExpand ())
;(struct idB BasicExpand (name))
;(struct bool BasicExpand (v))
;(struct letB BasicExpand (bindings body))
;(struct lambdaB BasicExpand (args body))



(define-type ProgramStructure
  [codeP (cs character-set?)]
  [errorP (cs character-set?)]
  [ifP (tst Facts?)
       (thn ProgramStructure?)
       (els ProgramStructure?)
       (cs  character-set?)])

(define (ProgramStructure-cs program)
  (cond
    [(codeP? program) (codeP-cs program)]
    [(errorP? program) (errorP-cs program)]
    [(ifP? program) (ifP-cs program)]))


(define-type Program*Scope
  [p*s (program BasicExpand?)
       (scope (hash/c symbol? VariableContent?))])
  
(define-type Facts
  [anyf]
  [nonef]
  ;[N/Af]
  [notf (i Facts?)]
  [andf (c Facts?)
        (i Facts?)]
  [orf (c Facts?)
       (i Facts?)]
  [complexf (program BasicExpand?)
            (scope (hash/c symbol? VariableContent?))]
  [predf (pred symbol?)
         (name symbol?)
         (scope (hash/c symbol? VariableContent?))]
  [complex-predf
   (pred symbol?)
   (name symbol?)
   (scope (hash/c symbol? VariableContent?))
   (innerApp appB?)]; applied on an AppB that only has one variable with the rest of the arguments being constants.
  [iff (tst Facts?)
       (thn Facts?)
       (els Facts?)])

(define-type VariableContent
  [func-arg]
  [sub-defined (def-expr BasicExpand?)])

;(define-type RelatedAndOtherFacts
;  [related-otherf (r Facts?)
;                  (o Facts?)]) 

(struct related-otherf (r o)
  #:guard (struct-guard/c Facts? Facts?)
  #:transparent
  )


;this function ascertains if the relevant source information on the syntax object goes with the path supplied
(define/contract (syntax->charset path-supplied syn)
  (-> path-string? syntax? character-set?)
  (define source (syntax-source syn))
  (define source-path (if (path? source)
                          source
                          (if (string? source)
                              (string->path source)
                              #f)))
  (define pos (syntax-position syn))
  (define span (syntax-span syn))
  (define path (if (string? path-supplied) (string->path path-supplied) path-supplied))
  (if
   (and (path? source-path) (path? path)
        (equal? (simplify-path source-path) (simplify-path path)))
      (character-set (for/set ([x (in-range pos (+ pos span))]) x))
      (empty-charset))); if path not part of source then an empty charset is returned. 

(define (syntax->symbol x)
  (if (symbol? x)
      x
      (let ([synobj (syntax-e x)])
        (if (list? synobj)
            (syntax->symbol (first synobj))
            (syntax->symbol synobj)))))
            

(define/contract (parse-syn path syn)
  (-> path-string? syntax? BasicExpand?)
  (define (parse-syn-with-path syn)
    (parse-syn path syn))
  (syntax-parse syn
    [x:boolean (bool (syntax-e #'x) (syntax->charset path syn))]
    [((~datum quote) #f) (bool #f (syntax->charset path syn))]
    [((~datum quote) #t) (bool #t (syntax->charset path syn))]
    [((~datum quote) x:number)  (num (syntax-e #'x) (syntax->charset path syn))]
    [((~datum quote) x)  (symbolB (syntax-e #'x) (syntax->charset path syn))]
    [x:number (num (syntax-e #'x) (syntax->charset path syn))]
    [((~datum #%top) . x:identifier) (idB (syntax-e #'x) (syntax->charset path syn))]
    [((~datum #%variable-reference) x:identifier) (idB (syntax-e #'x) (syntax->charset path syn))]
    [x:identifier (idB (syntax-e #'x) (syntax->charset path syn))]
    [((~datum #%app) (~datum error) _ ...) (errorB (syntax->charset path syn))]
    [((~datum #%app) (~datum raise-argument-error) _ ...) (errorB (syntax->charset path syn))]
    [((~datum #%app) (~datum not) inner) (appNotB (parse-syn path #'inner) (syntax->charset path syn)) ]
    [((~datum #%app) name values ...) (appB (parse-syn path #'name) (map parse-syn-with-path (syntax-e #'(values ...))) (syntax->charset path syn))]
    [((~datum let-values) ([(ids) val-exprs] ...) body)  (letB
                                                          (for/list ([id (syntax-e #'(ids ...))]
                                                                     [val-expr (syntax-e #'(val-exprs ...))])
                                                            (cons (syntax-e id) (parse-syn path val-expr)))
                                                          (parse-syn path #'body)
                                                          (syntax->charset path syn))]
    [((~datum let-values) _ ...)  (error `parse-syn "tool doesn't support let-values with multiple values")]
    [((~datum if) tst thn els)  (ifB (parse-syn path #'tst) (parse-syn path #'thn) (parse-syn path #'els) (syntax->charset path syn))]
    [((~datum lambda) (arg ...) body) (lambdaB (map syntax->datum (syntax-e #'(arg ...))) (parse-syn path #'body) (syntax->charset path syn))]
   ; [(syms ...+) (map parse-syn-with-path (syntax-e #'(syms ...)))]
    ))


; each instance of this struct shall have the necessary removal spanset, the contract statement and the relevant position information on the start and the end 
(serializable-struct func-contract-info (func-name path spanset contract define-end body-start desirability)
  #:guard (struct-guard/c any/c path-string? character-set? any/c integer? integer? integer?))

(serializable-struct contract-infos&errors (i e))


;(define-type FuncContractInfo
;  [func-contract-info (spanset character-set?)
;                      (contract any/c)
;                      (define-end positive?)
;                      (body-start positive?)]) 

;(define-type ContractsAndUnusedFacts
;  [contracts&unused (c any/c)
;                    (f Facts?)])


;(define (parse s-expr)
;  (match s-expr
;    [(? number? x) (num x)]
;    [(? symbol? x) (idB x)]
;    [`(#%top . ,x) (idB x)]
;    [`#f (bool #f)]
;    [`#t (bool #t)]
;    [''#f (bool #f)]
;    [''#t (bool #t)]
;    [`(#%app error ,_ ...) (errorB)]
;    [`(#%app not ,x) (appNotB (parse x))]
;    [`(#%app ,name ,values ...) (appB (parse name) (map parse values))]
;    [(list `let (list (and pairs (cons _ _)) ...) body) (letB
;                                                         (for/list ([pair pairs])
;                                                           (cons (car pair) (parse (first(cdr pair)))))
;                                                         (parse body))]
;    [`(let-values ([(,ids) ,val-exprs] ...) ,body) (letB
;                                                    (for/list ([id ids]
;                                                               [val-expr val-exprs])
;                                                      (cons id (parse val-expr)))
;                                                    (parse body))]
;    [`(if ,tst ,thn ,els)  (ifB (parse tst) (parse thn) (parse els))]
;    [`(lambda (,arg ...) ,body) (lambdaB arg (parse body))]))

