#lang racket

(provide bin)

(define (bin n l)
  (~a (number->string n 2) #:min-width l #:align 'right #:left-pad-string "0"))
