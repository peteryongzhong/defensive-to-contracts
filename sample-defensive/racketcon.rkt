#lang racket
(require "../pcond.rkt")
(require "racketcon_.rkt")



(define (store-url-and-ranking url ranking)
  (unless (and (exact-integer? ranking) (positive? ranking))
    (error "ranking not valid"))
  (pcond
   [(url? url)
        (if (or (url-secure? url) (url-local? url))
            (somecode)
            (error "Url resources must be secured or located locally"))]
   [(string? url)
    (someothercode)]
   [else (error "invalid URL input")]))
    