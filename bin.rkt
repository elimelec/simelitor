#lang racket

(require threading)

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

(define (sdec n)
  (let ([sign (substring n 0 1)]
        [number (substring n 1)])
    (match sign
      ["0" (dec number)]
      ["1" (~> number bitstring-not dec add1 -)])))

(define (bitstring-not s)
  (regexp-replace* #px"[01]" s
                   (lambda (s) (if (string=? s "0") "1" "0"))))

(define (bitstring-xor s1 s2)
  (let ([l1 (string->list s1)]
        [l2 (string->list s2)])
    (list->string (for/list ([c1 l1] [c2 l2])
                    (if (char=? c1 c2) #\0 #\1)))))

(define (bitstring-or s1 s2)
  (let ([l1 (string->list s1)]
        [l2 (string->list s2)])
    (list->string (for/list ([c1 l1] [c2 l2])
                    (if (or (char=? c1 #\1) (char=? c2 #\1)) #\1 #\0)))))

(define (bitstring-and s1 s2)
  (let ([l1 (string->list s1)]
        [l2 (string->list s2)])
    (list->string (for/list ([c1 l1] [c2 l2])
                    (if (or (char=? c1 #\0) (char=? c2 #\0)) #\0 #\1)))))
