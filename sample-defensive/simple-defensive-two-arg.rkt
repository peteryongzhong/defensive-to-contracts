#lang plai

(define (simple-defensive-two-arg arg0 arg1)
      (if (not (real? arg0))
          (error "arg0 needs to be real")
          arg0))