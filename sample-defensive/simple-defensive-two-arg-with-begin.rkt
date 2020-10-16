#lang plai

(define (simple-defensive-two-arg arg0 arg1)
  (unless (negative? arg0)
    (error "arg0 need to be positive"))
  (if (not (real? arg0))
      (error "arg0 needs to be real")
      arg0))