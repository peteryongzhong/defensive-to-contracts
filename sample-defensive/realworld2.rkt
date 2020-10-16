#lang racket

(define (realworld2 msg tmp-port)
  (cond
    [(and (path-string? msg) (file-exists? msg))
     (call-with-input-file msg
       (lambda (msg-port)
         (copy-port msg-port tmp-port)))]
    [(string? msg)
     (displayln (append msg) tmp-port)]
    [else
     (raise-argument-error 'mutt "(or/c file-exists? content?)" msg)]))