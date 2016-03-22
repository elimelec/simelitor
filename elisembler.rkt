#lang racket

(require threading)
 (require racket/hash)

(define (trim-comments s)
  (regexp-replace #rx";.*$" s ""))

(define (format-code s)
  ((compose1 string-trim trim-comments) s))

;; match full regex on instruction

(define ins
  (map pregexp '("clc" "clv" "clz" "cls" "ccc" "sec" "sev"
    "sez" "ses" "scc" "nop" "ret" "reti" "halt"
    "wait" "push pc" "pop pc" "push flag")))
(define ops
  '("1100000000000000" "1100001000000000" "1100010000000000"
    "1100011000000000" "1100100000000000" "1100101000000000"
    "1100110000000000" "1100111000000000" "1101000000000000"
    "1101001000000000" "1110000000000000" "1110001000000000"
    "1110010000000000" "1110011000000000" "1110100000000000"
    "1110101000000000" "1110110000000000" "1110111000000000"))

(define insops
  (for/list ([i (length ins)])
    (cons (list-ref ins i) (list-ref ops i))))

(define hinsops (make-hash insops))

(define ins2
  #hash(
        (#px"add r([0-9]{1,2}), r([0-9]{1,2})" . "000101(fill-bin \\1 4)01(fill-bin \\2 4)")
        (#px"sub r([0-9]{1,2}), r([0-9]{1,2})" . "001011(fill-bin \\1 4)01(fill-bin \\2 4)")

        (#px"inc r([0-9]{1,2})" . "100001000001(fill-bin \\1 4)")
        ))

(define instructions (hash-union ins2 hinsops))

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
