#lang racket

(require threading)

(define (trim-comments s)
  (regexp-replace #rx";.*$" s ""))

(define (format-code s)
  ((compose1 string-trim trim-comments) s))

;; match full regex on instruction

(define instructions
  #hash(
        (#px"add r([0-9]{1,2}), r([0-9]{1,2})" . "000101(fill-bin \\1 4)01(fill-bin \\2 4)")
        (#px"sub r([0-9]{1,2}), r([0-9]{1,2})" . "001011(fill-bin \\1 4)01(fill-bin \\2 4)")

        (#px"inc r([0-9]{1,2})" . "100001000001(fill-bin \\1 4)")

        (#px"clc" . "1100000000000000")
        (#px"clv" . "1100001000000000")

        (#px"nop" . "1110000000000000")
        ))

(define (strip-whitespace s)
  (regexp-replace* #rx" " s ""))

(define (bin n)
  (cond
    [(< n 2) (number->string n)]
    [else (string-append (bin (quotient n 2)) (number->string (remainder n 2)))]))

(define (fill-bin n l)
  (~a (bin n) #:min-width l #:align 'right #:left-pad-string "0"))

(define (translate-instruction i)
  (regexp-replace*
   #rx"\\(.*?\\)"
   i
   (λ (s) (eval (call-with-input-string s read)))))

(define (first-hash i)
  (for/first ([r (hash-keys instructions)]
              #:when (regexp-match? r i))
    r))

(define (syntax-ok? code)
  (not (member #f (map first-hash code))))

(define (first-pass code)
  (let* ([code code]
         [regexes (map first-hash code)]
         [replaces (map (λ (r) (hash-ref instructions r)) regexes)])
    (for/list ([i (length code)])
      (regexp-replace (list-ref regexes i)
                      (list-ref code i)
                      (list-ref replaces i)))))

(define (second-pass code)
  (map translate-instruction code))

(define (compile-asm filename)
  (~> filename
      file->lines
      (map format-code _)
      first-pass
      second-pass))
      ; (display-lines-to-file _ (string-replace filename ".s" ".obj"))))
