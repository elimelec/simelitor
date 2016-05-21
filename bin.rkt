#lang racket

(provide (all-defined-out))

(define bin
  (lambda (n [l 16])
    (~a (number->string n 2) #:min-width l #:align 'right #:left-pad-string "0")))

(define (dec n)
  (string->number n 2))

(define (bitstring-not s)
  (regexp-replace* #px"[01]" s
                   (lambda (s) (if (string=? s "0") "1" "0"))))
