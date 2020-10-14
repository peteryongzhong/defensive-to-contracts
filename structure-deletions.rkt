#lang racket
(require (only-in plai define-type test print-only-errors))
(require "definitions.rkt")
(require "chspans.rkt")

(define/contract (structure-deletions structure)
  (-> ProgramStructure? character-set?)
  (define (collapse-simplify structure)
    (match structure
      [(ifP tst (errorP _) (errorP _) chs) (errorP chs)]
      [(ifP tst (codeP _)  (codeP _) chs) (codeP chs)]
      [(ifP tst thn els chs) (ifP tst (collapse-simplify thn) (collapse-simplify els) chs) ]
      [_ structure]))
   
  (define (progressively-collapse-structure structure)
    (let ([new-structure (collapse-simplify structure)])
      (if (equal? new-structure structure)
          new-structure
          (progressively-collapse-structure new-structure))))

  (match  (progressively-collapse-structure structure)
    [(codeP _) (empty-charset)]
    [(errorP cs) cs]
    [(ifP _ (errorP _) (codeP c-chs)  chs)
     (subtract-charset chs c-chs)]
    [(ifP _ (codeP c-chs) (errorP _)  chs)
     (subtract-charset chs c-chs)]
    [(ifP _ (errorP _) innerifp chs)
     (define inner-char-set (structure-deletions innerifp))
     (union-charset (subtract-charset chs (ProgramStructure-cs innerifp))
                    inner-char-set)]
    [(ifP _ innerifp (errorP _) chs)
     (define inner-char-set (structure-deletions innerifp))
     (union-charset (subtract-charset chs (ProgramStructure-cs innerifp))
                    inner-char-set)]
    [(ifP _ (codeP _) innerifp  chs) (structure-deletions innerifp)]
    [(ifP _ innerifp (codeP _) chs) (structure-deletions innerifp)]
    [(ifP _ innerifp1 innerifp2 chs)  (union-charset (structure-deletions innerifp1 )
                                                     (structure-deletions innerifp2))]))
(provide structure-deletions)
      
(module+ test
  (print-only-errors)
  (test (structure-deletions 
         (ifP (predf 'pred1 'arg0 '#hash())
              (codeP (charset 21 15))
              (errorP (charset 41 26))
              (charset 0 73)))
        (charset-fromsets (list (charset 0 21) (charset 36 37))))
  (test  (structure-deletions 
                        (ifP (predf 'pred1 'arg0 '#hash())
                             (errorP (charset 21 26))
                             (codeP (charset 52 15))
                             (charset 0 69)))
                   (charset-fromsets (list (charset 0 52) (charset 67 2)))
        )
  (test (structure-deletions 
                        (ifP (predf 'pred1 'arg0 '#hash())
                             (ifP (predf 'pred2 'arg0 '#hash())
                                  (errorP (charset 46 26))
                                  (errorP (charset 81 34))
                                  (charset 21 95))
                             (codeP (charset 121 19))
                             (charset 0 146)))
                       (charset-fromsets (list (charset 0 121) (charset 140 6))))
        
  (test  (structure-deletions 
                        (ifP (predf 'pred1 'arg0 '#hash())
                             (ifP (predf 'pred2 'arg0 '#hash())
                                  (codeP (charset 46 26))
                                  (codeP (charset 81 34))
                                  (charset 21 95))
                             (errorP (charset 116 19))
                             (charset 0 135)))
                       (charset-fromsets (list (charset 0 21) (charset 116 19))))
     

  (test  (structure-deletions 
                        (ifP (predf 'pred1 'arg0 '#hash())
                             (ifP (predf 'pred2 'arg0 '#hash())
                                  (codeP (charset 46 25))
                                  (errorP (charset 71 34))
                                  (charset 21 94))
                             (ifP (predf 'pred3 'arg0 '#hash())
                                  (errorP (charset 145 34))
                                  (codeP (charset 188 25))
                                  (charset 120 94))
                             (charset 0 215)))
                   (charset-fromsets (list (charset 21 25) (charset 71 44) (charset 120 68) (charset 213 1))))
 
;
  (test(structure-deletions 
                        (ifP (predf 'pred1 'arg0 '#hash())
                             (errorP (charset 20 34))
                             (ifP (predf 'pred3 'arg0 '#hash())
                                  (errorP (charset 84 34))
                                  (codeP (charset 127 25))
                                  (charset 59 94))
                             (charset 0 154)))
                       (charset-fromsets (list (charset 0 127) (charset 152 2))))

  (test  (structure-deletions 
                        (ifP (predf 'pred1 'arg0 '#hash())
                             (ifP (predf 'pred2 'arg0 '#hash())
                                  (codeP (charset 46 25))
                                  (errorP (charset 80 34))
                                  (charset 21 94))
                             (errorP (charset 120 34))
                             (charset 0 155)))
                       (charset-fromsets (list (charset 0 46) (charset 71 84))))
   
  (test(structure-deletions 
                    (ifP (predf 'pred1 'arg0 '#hash())
                         (codeP (charset 21 25))
                         (ifP (predf 'pred3 'arg0 '#hash())
                              (errorP (charset 76 34))
                              (codeP (charset 119 25))
                              (charset 51 94))
                         (charset 0 146)))
                   (charset-fromsets (list (charset 51 68) (charset 144 1))))
)

  

;  (test (charop-equal? (structure-deletions 
;                        (ifP (predf 'pred1 'arg0 '#hash())
;                             (ifP (predf 'pred2 'arg0 '#hash())
;                                  (codeP (chspan 46 25))
;                                  (errorP (chspan 80 34))
;                                  (chspan 21 94))
;                             (codeP
;                                  (chspan 188 25))
;                             (chspan 0 215)))
;                       (choperations (list (chspan 21 94) ) (list (chspan 46 25))))
;        #t)
;  )
;  