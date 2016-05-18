#lang racket

(provide bin dec)

(define bin
  (lambda (n [l 16])
    (~a (number->string n 2) #:min-width l #:align 'right #:left-pad-string "0")))

(define (dec n)
  (string->number n 2))
