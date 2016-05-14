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
                      ("INDEX0" . "000")
                      ("INDEX1" . "001")
                      ("INDEX2" . "010")
                      ("INDEX3" . "011")
                      ("INDEX4" . "100")
                      ("INDEX5" . "101")
                      ("INDEX6" . "110")
                      ("INDEX7" . "111")
                      )
                #hash(
                      ("nT" . "0")
                      ("F" . "1")
                      )
                )
  )

(define lines (file->lines "microprogram_emulare_text.txt"))
(set! lines (map (lambda (s) (string-replace s "\t" "")) lines))
(set! lines (map (lambda (s) (string-replace s ":" ":,")) lines))
(set! lines (map (lambda (s) (string-split s ",")) lines))


(define labels
  (for/hash ([i (length lines)]
             [line lines]
             #:when (string-contains? (car line) ":"))
    (values (car line) i)))

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
          (hash-ref (list-ref codes i) instr instr)
          )))

(set! lines
      (map string-join lines))