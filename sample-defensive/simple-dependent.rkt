#lang plai

(define (func arg0 arg1)
  (unless (< arg0 arg1)
    (error "arg0 >= arg1"))
  (if (not (exact? arg0))
      (error "arg0 needs to be exact")
      arg0))