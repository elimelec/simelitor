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

(define (strip-whitespace s)
  (regexp-replace* #rx" " s ""))

(define (syntax-ok? code)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (compile-asm code)
    #t))

(define opcodes
  #hash(["mov" . "0000"]
        ["add" . "0001"]
        ["sub" . "0010"]
        ["cmp" . "0011"]
        ["and" . "0100"]
        ["or" . "0101"]
        ["xor" . "0110"]

        ["clr" . "1000000000"]
        ["neg" . "1000001000"]
        ["inc" . "1000010000"]
        ["dec" . "1000011000"]
        ["asl" . "1000100000"]
        ["asr" . "1000101000"]
        ["lsr" . "1000110000"]
        ["rol" . "1000111000"]
        ["ror" . "1001000000"]
        ["rlc" . "1001001000"]
        ["rrc" . "1001010000"]
        ["jmp" . "1001011000"]
        ["call" . "1001100000"]
        ["push" . "1001101000"]
        ["pop" . "1001110000"]

        ["br" . "10100000"]
        ["bne" . "10100010"]
        ["beq" . "10100100"]
        ["bpl" . "10100110"]
        ["bmi" . "10101000"]
        ["bcs" . "10101010"]
        ["bcc" . "10101100"]
        ["bvs" . "10101110"]
        ["bvc" . "10110000"]

        ["clc" . "1110000000000000"]
        ["clv" . "1110000100000000"]
        ["clz" . "1110001000000000"]
        ["cls" . "1110001100000000"]
        ["ccc" . "1110010000000000"]
        ["sec" . "1110010100000000"]
        ["sev" . "1110011000000000"]
        ["sez" . "1110011100000000"]
        ["ses" . "1110100000000000"]
        ["scc" . "1110100100000000"]
        ["nop" . "1110101000000000"]
        ["ret" . "1110101100000000"]
        ["reti" . "1110110000000000"]
        ["halt" . "1110110100000000"]
        ["wait" . "1110111000000000"]
        ["push-pc" . "1110111100000000"]
        ["pop-pc" . "1111000000000000"]
        ["push-flag" . "1111000100000000"]
        ["pop-flag" . "1111001000000000"]))

(define (temp-pass instruction)
  (match instruction
    [(pregexp #px"([a-z]+) r([0-9]{1,2}), r([0-9]{1,2})" (list _ op r1 r2))
     (string-append
      (hash-ref opcodes op)
      (string-append "01" (bin (string->number r2) 4))
      (string-append "01" (bin (string->number r1) 4)))]
    [(pregexp #px"([a-z]+) r([0-9]{1,2}), ([0-9-]{1,5})" (list _ op r1 n))
     (string-append
      (hash-ref opcodes op)
      (string-append "00" "0000")
      (string-append "01" (bin (string->number r1) 4))
      (sbin (string->number n)))]
    [(pregexp #px"([a-z]+) r([0-9]{1,2})" (list _ op r2))
     (string-append
      (hash-ref opcodes op)
      (string-append "01" (bin (string->number r2) 4)))]
    [(pregexp #px"([a-z-]+)" (list _ op))
     (string-append
      (hash-ref opcodes op))]))

(define (compile-asm-file filename)
  (~> filename
      file->lines
      (map format-code _)
      (map temp-pass _)
      (map string-split-at-16 _)
      flatten))
(define (compile-asm list)
  (~> list
      (map format-code _)
      (map temp-pass _)
      (map string-split-at-16 _)
      flatten))
