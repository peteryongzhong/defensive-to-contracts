#lang racket

;(require (for-syntax racket/base))
;(define-syntax (pcond stx)
;    (syntax-case stx ()
;      [(pcond (else outcome)) (equal? (syntax->datum #'else) `else) (syntax-property #'outcome `condelse #t)]
;      [(pcond (tst outcome)) (syntax-property
;                              #'(if tst
;                                   outcome
;                                   (void))
;                              `condbranch
;                              #t)]
;      [(pcond (tst outcome) clauses ...) 
;       (syntax-property
;        #'(if tst
;              outcome
;              (pcond clauses ...))
;        `condbranch
;        #t)]))



(require (for-syntax syntax/parse syntax/srcloc))
   
(define-syntax (pcond stx)
  (define (process-syn stx original-syn)
    (syntax-parse stx
      [(pcond (~and clause ((~datum else) outcome)))
       (define position (source-location-position #'clause))
       (define span (source-location-span  #'clause))
       (define source (source-location-source #'clause))
       (update-source-location
        (syntax-property #'(if #t outcome (void)) `condelse #t)
        #:source source
        #:position position
        #:span span)
        ]
      [(pcond (~and clause (tst outcome)))
       (define original-position (source-location-position original-syn))
       (define original-span (source-location-span original-syn))
       (define source (source-location-source #'clause))
       (define position (source-location-position #'clause))
       (define span (- (source-location-end original-syn) position 1))
       (if (equal? stx original-syn)
           (update-source-location
            (syntax-property
             #'(if tst
                   outcome
                   (void))
             `condbranch
             #t)
            #:source source 
            #:position original-position
            #:span original-span)
           (let ([processed-outcome
                  (update-source-location
                   #'outcome
                   #:position position
                   #:span (source-location-span #'clause))])
             (update-source-location
              (syntax-property
               #`(if tst
                     #,processed-outcome
                     (void))
               `condbranch
               #t)
              #:source source
              #:position position
              #:span span)))]
      [(pcond (~and clause (tst outcome)) clauses ...)
       (define source (source-location-source #'clause))
       (define position (source-location-position #'clause))
       (define span (- (source-location-end original-syn) position 1))
       (define processed-outcome
         (update-source-location
          #'outcome
          #:position position
          #:span (source-location-span #'clause)))
       (define inner
         (update-source-location 
          #`(if #t
                outcome
                (void))
          #:source source
          #:position (source-location-position #'outcome)
          #:span (source-location-span #'outcome)))
       (update-source-location  #`(if tst
                                      #,inner
                                      #,(process-syn #`(pcond clauses ...) original-syn))
                                      #:source source
                                      #:position position
                                      #:span span)  ]))
  (process-syn stx stx)
  )

(provide pcond)