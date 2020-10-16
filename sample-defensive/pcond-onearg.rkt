#lang racket
(require "../pcond.rkt")


;(->i ((arg1 any/c) (arg2 any/c)) #:pre (arg1 arg2) (and (or (number? arg1) (number? arg2)) (or (number? arg2) (number? arg1))) any)
(define (sample arg1 arg2)
  (pcond [(and (number? arg1)
               (number? arg2)) (+ arg1 arg2)]
         [(number? arg1) (+ arg1 (string->number arg2))]
         [(number? arg2) (+ arg2 (string->number arg1))]
         [else (error "error")]))