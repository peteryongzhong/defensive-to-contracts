#lang plai

(define (cond-one-args arg0)
  (unless (string? arg0)
    (error "must be string"))
  (length arg0)
  )