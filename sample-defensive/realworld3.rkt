#lang racket

(define (number x y round?)
  (unless (and (number? x) (< -1e21 x 1e21))
    (raise-argument-error 'number "valid number" x))
  (let ([x (if round? (/ (round (* x 1e6)) 1e6) x)])
    (number->string (if (integer? x)
                        (inexact->exact x)
                        x))))