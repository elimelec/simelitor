#lang racket

(provide (all-defined-out))

(define bin
  (lambda (n [l 16])
    (when (< n 0)
      (raise "n can't be negative"))
    (~a (number->string n 2) #:min-width l #:align 'right #:left-pad-string "0")))

(define sbin
  (lambda (n [l 16])
    (if (< n 0)
        (let ([n (sub1 (abs n))])
          (let ([bin-n (~a (number->string n 2) #:min-width l #:align 'right #:left-pad-string "0")])
            (bitstring-not bin-n)))
        (~a (number->string n 2) #:min-width l #:align 'right #:left-pad-string "0"))))

(define (dec n)
  (string->number n 2))

(define (bitstring-not s)
  (regexp-replace* #px"[01]" s
                   (lambda (s) (if (string=? s "0") "1" "0"))))
