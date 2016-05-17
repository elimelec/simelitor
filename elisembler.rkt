#lang racket

(require threading)
(require "bin.rkt")

(provide compile-asm-file
         compile-asm
         syntax-ok?)

(define (string-split-at-16 s)
  (for/list ([i (/ (string-length s) 16)])
    (substring s (* i 16) (* (+ i 1) 16))))

(define (trim-comments s)
  (regexp-replace #rx";.*$" s ""))

(define (format-code s)
  ((compose1 string-trim trim-comments) s))

;; match full regex on instruction

(define instructions
  #hash(
        (#px"add r([0-9]{1,2}), r([0-9]{1,2})" . "000101(bin \\1 4)01(bin \\2 4)")
        (#px"sub r([0-9]{1,2}), r([0-9]{1,2})" . "001011(bin \\1 4)01(bin \\2 4)")

        (#px"add r([0-9]{1,2}), ([0-9]{1,5})" . "000101(bin \\1 4)000000(bin \\2 16)")

        (#px"inc r([0-9]{1,2})" . "100001000001(bin \\1 4)")

        (#px"nop" . "1110000000000000")
        (#px"wait" . "1110100000000000")
        (#px"sec" . "1100101000000000")
        (#px"scc" . "1101001000000000")
        (#px"halt" . "1110011000000000")
        (#px"sez" . "1100111000000000")
        (#px"sev" . "1100110000000000")
        (#px"reti" . "1110010000000000")
        (#px"clc" . "1100000000000000")
        (#px"clz" . "1100010000000000")
        (#px"clv" . "1100001000000000")
        (#px"cls" . "1100011000000000")
        (#px"ccc" . "1100100000000000")
        (#px"ret" . "1110001000000000")
        (#px"ses" . "1101000000000000")
        (#px"push flag" . "1110111000000000")
        (#px"pop flag" . "1111000000000000")
        (#px"push pc" . "1110101000000000")
        (#px"pop pc" . "1110110000000000")
        ))

(define (strip-whitespace s)
  (regexp-replace* #rx" " s ""))

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

(define (compile-asm-file filename)
  (~> filename
      file->lines
      (map format-code _)
      first-pass
      second-pass
      (map string-split-at-16 _)
      flatten))
(define (compile-asm list)
  (~> list
      (map format-code _)
      first-pass
      second-pass
      (map string-split-at-16 _)
      flatten))
