#lang racket

(define codes '(
                #hash(
                      ("NONE" . "0000")
                      ("PdIR[Offset]s" . "0001")
                      ("PdFLAGs" . "0010")
                      ("PdSPs" . "0011")
                      ("PdTs" . "0100")
                      ("PdnotTs" . "0101")
                      ("PdPCs" . "0110")
                      ("PdIVRs" . "0111")
                      ("PdADRs" . "1000")
                      ("PdMDRs" . "1001")
                      ("PdRGs" . "1010")
                      ("Pd0s" . "1011")
                      ("Pd-1s" . "1100")
                      ("Pd1s" . "1101")
                      )
                #hash(
                      ("NONE" . "0000")
                      ("PdIR[Offset]d" . "0001")
                      ("PdFLAGd" . "0010")
                      ("PdSPd" . "0011")
                      ("PdTd" . "0100")
                      ("PdnotTd" . "0101")
                      ("PdPCd" . "0110")
                      ("PdIVRd" . "0111")
                      ("PdADRd" . "1000")
                      ("PdMDRd" . "1001")
                      ("PdRGd" . "1010")
                      ("Pd0d" . "1011")
                      )
                #hash(
                      ("NONE" . "0000")
                      ("SUM" . "0001")
                      ("AND" . "0010")
                      ("OR" . "0011")
                      ("XOR" . "0100")
                      ("ASL" . "0101")
                      ("ASR" . "0110")
                      ("LSR" . "0111")
                      ("ROL" . "1000")
                      ("ROR" . "1001")
                      ("RLC" . "1010")
                      ("RRC" . "1011")
                      ("notDBUS" . "1100")
                      )
                #hash(
                      ("NONE" . "0000")
                      ("PmFLAG" . "0001")
                      ("PmRG" . "0010")
                      ("PmSP" . "0011")
                      ("PmT" . "0100")
                      ("PmPC" . "0101")
                      ("PmIVR" . "0110")
                      ("PmADR" . "0111")
                      ("PmMDR" . "1000")
                      )
                #hash(
                      ("NONE" . "00000")
                      ("PdCOND" . "00001")
                      ("CinPdCOND" . "00010")
                      ("plus2SP" . "00011")
                      ("minus2SP" . "00100")
                      ("plus2PC" . "00101")
                      ("A(0)BPO" . "00110")
                      ("A(0)C" . "00111")
                      ("A(1)C" . "01000")
                      ("A(0)V" . "01001")
                      ("A(1)V" . "01010")
                      ("A(0)Z" . "01011")
                      ("A(1)Z" . "01100")
                      ("A(0)S" . "01101")
                      ("A(1)S" . "01110")
                      ("A(0)CVZS" . "01111")
                      ("A(1)CVZS" . "10000")
                      ("A(0)BVI" . "10001")
                      ("A(1)BVI" . "10010")
                      )
                #hash(
                      ("NONE" . "00")
                      ("IFCH" . "01")
                      ("READ" . "10")
                      ("WRITE" . "11")
                      )
                #hash(
                      ("NONE" . "0000")
                      ("INT" . "0001")
                      ("C" . "0010")
                      ("Z" . "0011")
                      ("S" . "0100")
                      ("V" . "0101")
                      ("AD" . "0110")
                      ("ACLOW" . "0111")
                      ("CIL" . "1000")
                      )
                #hash(
                      ("(INDEX0)" . "000")
                      ("(INDEX1)" . "001")
                      ("(INDEX2)" . "010")
                      ("(INDEX3)" . "011")
                      ("(INDEX4)" . "100")
                      ("(INDEX5)" . "101")
                      ("(INDEX6)" . "110")
                      ("(INDEX7)" . "111")
                      )
                #hash(
                      ("nT" . "0")
                      ("F" . "1")
                      )
                )
  )

(define (bin n)
  (cond
    [(< n 2) (number->string n)]
    [else (string-append (bin (quotient n 2)) (number->string (remainder n 2)))]))

(define (fill-bin n l)
  (~a (bin n) #:min-width l #:align 'right #:left-pad-string "0"))


(define (that-thing-with-jump instr)
  (let ([line (string-split instr)])
       (for/list ([instr line] [i (length line)])
         (cond
          [(and (= i 0) (string=? instr "0000"))
           "0000 000 0 00000000"]
          [(and (= i 0) (string=? instr "NONE"))
           "0000"]
          [(and (= i 2) (string-contains? instr "INDEX"))
           (string-append (hash-ref (list-ref codes 7) instr) " 0")]
          [(and (= 3 (length line)) (= i 2) (string=? (list-ref line 1) "JUMP"))
           (string-append "000 0 " (fill-bin (hash-ref labels instr) 8))]
          [(and (= 4 (length line)) (= i 3) (string=? (list-ref line 1) "JUMPI"))
           (fill-bin (hash-ref labels instr) 8)]
          [(and (= 6 (length line)) (= i (- (length line) 1)) (string=? instr "STEP"))
            (fill-bin (hash-ref labels (list-ref line 3)) 8)]
          [(and (= 6 (length line)) (= i (- (length line) 2)) (string=? instr "ELSE"))
            (if (string-prefix? (list-ref line 1) "N") "1" "0")]
          [(and (= 6 (length line)) (= i 1))
            (hash-ref
             (list-ref codes 6)
             (if (string-prefix? instr "N")
                 (string-replace instr "N" "") instr))]
          [(and (= 6 (length line)) (= i 2) (string=? instr "JUMP"))
           "000"]
          [(and (not (= 2 (length line))) (= i (- (length line) 1)) (string=? instr "STEP"))
            (fill-bin 0 8)]
          [(and (= i 1) (= 2 (length line)) (string=? (list-ref line 0) "NONE") (string=? instr "STEP"))
           (string-append "000 1 " (fill-bin 0 8))]
          [else ""]))))

(define (jumpi-trans instr)
  (let ([s (string-join (that-thing-with-jump instr))])
    (if (= (string-length (string-replace s " " "")) 16)
        s
        (raise (string-append instr " => " s))
        ;s
        )))

(define lines (file->lines "microprogram_emulare_text.txt"))
(set! lines (map (lambda (s) (string-replace s "\t" "")) lines))
(define orig-lines lines)
(set! lines (map (lambda (s) (string-replace s ":" ":,")) lines))
(set! lines (map (lambda (s) (string-split s ",")) lines))


(define labels
  (for/hash ([i (length lines)]
             [line lines]
             #:when (string-contains? (car line) ":"))
    (values (string-replace (car line) ":" "") i)))

; I have no idea why it doesn't work with one pass
(set! lines
      (for/list ([line lines])
        (for/list ([instr line]
                   [i (length line)]
                   #:when (not (string-contains? instr ":")))
          (hash-ref (list-ref codes i) instr instr)
          )))

(set! lines
      (for/list ([line lines])
        (for/list ([instr line]
                   [i (length line)]
                   #:when (not (string-contains? instr ":")))
          (if (= i (- (length line) 1))
           (jumpi-trans instr)
           (hash-ref (list-ref codes i) instr instr))
          )))
        
(set! lines (map string-join lines))
(set! lines (map (lambda (s) (string-replace s " " "")) lines))

(for/list ([i (length lines)]
             #:when (not (= 39 (string-length (list-ref lines i)))))
    (list
     i
     (string-length (list-ref lines i))
     (list-ref orig-lines i)
     (list-ref lines i)))
