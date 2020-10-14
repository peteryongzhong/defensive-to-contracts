#lang racket
(require racket/serialize)


(define (test result actual)
  (if (equal? result actual)
      (print "test passed")
      (error "test did not pass ~a" result)))

(serializable-struct character-set (s)
    #:guard (struct-guard/c set?)
    #:methods
    gen:equal+hash
    [(define (equal-proc a b equal?-recur)
       (equal?-recur (character-set-s a)
                     (character-set-s b)))
     (define (hash-proc a hash-recur)
       ; compute primary hash code of a
       (hash-recur (character-set-s a)))
     (define (hash2-proc a hash2-recur)
       ; compute secondary hash code of a
       (hash2-recur (character-set-s a)))])

(struct chspan (p s); position, span
  #:guard (struct-guard/c exact-nonnegative-integer? exact-nonnegative-integer?))

(define (charset-size charset)
  (set-count (character-set-s charset)))

(define (charset-empty? charset)
  (equal? charset (empty-charset)))

; this function create a charset from position pos spaning span. Note that the indicies start at 1. 
(define (charset pos span)
  (character-set (for/set ([x (in-range pos (+ pos span))]) x)))

; this function creates a charset with no characters
(define (empty-charset)
  (character-set (set)))

; this function merge two character sets and union the two underlying sets
(define (union-charset cs1 cs2)
  (character-set (set-union (character-set-s cs1)  (character-set-s cs2))))

; This function returns a character set with characters included in cs1 but not in cs2
(define (subtract-charset cs1 cs2)
  (character-set (set-subtract (character-set-s cs1)  (character-set-s cs2))))


(define (charset=? charset1 charset2)
  (equal? (character-set-s charset1) (character-set-s charset2)))

; union multiple character sets
(define (charset-fromsets sets)
  (foldl union-charset (empty-charset) sets))

(provide
 (struct-out character-set)
 charset
 empty-charset
 union-charset
 subtract-charset
 charset=?
 charset-empty?
 charset-fromsets
 charset-size)

(module+ test
  (test
   (charset=? (empty-charset)
              (character-set (set)))
   #t)
  (test
   (charset=? (charset 1 3)
              (character-set (set 1 2 3)))
   #t)
  (test
   (charset=? (union-charset (charset 1 3) (charset 2 4) )
              (character-set (set 1 2 3 4 5)))
   #t)

  (test
   (charset=? (subtract-charset (charset 1 3) (charset 2 4) )
              (character-set (set 1)))
   #t)

  (test
   (charset=? (subtract-charset (charset 1 3) (charset 4 5) )
              (character-set (set 1 2 3)))
   #t)
  )
