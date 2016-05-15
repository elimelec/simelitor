#lang racket

(provide bin
         fill-bin)

(define (bin n)
  (cond
    [(< n 2) (number->string n)]
    [else (string-append (bin (quotient n 2)) (number->string (remainder n 2)))]))

(define (fill-bin n l)
  (~a (bin n) #:min-width l #:align 'right #:left-pad-string "0"))