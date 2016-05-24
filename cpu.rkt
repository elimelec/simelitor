#lang racket

(require "bin.rkt")

(provide (all-defined-out))

(struct cpu (registers
             flag sp t pc ivr adr mdr ir mar mir
             memory
             microprogram state
             sbus dbus rbus) #:mutable)

(define (make-cpu)
  (define zero (bin 0 16))
  (cpu (make-vector 16 zero)
       zero zero zero zero zero zero zero zero zero (bin 0 64)
       (make-vector 65536 zero)
       (make-vector 138 (bin 0 64)) 0
       zero zero zero))

(define (registers)
  (cpu-registers a-cpu))
(define (register n)
  (vector-ref (cpu-registers a-cpu) n))
(define (set-register! n value)
  (vector-set! (cpu-registers a-cpu) n value))

(define (flag)
  (cpu-flag a-cpu))
(define (set-flag! value)
  (set-cpu-flag! a-cpu value))

(define (sp)
  (cpu-sp a-cpu))
(define (set-sp! value)
  (set-cpu-sp! a-cpu value))

(define (t)
  (cpu-t a-cpu))
(define (set-t! value)
  (set-cpu-t! a-cpu value))

(define (pc)
  (cpu-pc a-cpu))
(define (set-pc! value)
  (set-cpu-pc! a-cpu value))

(define (ivr)
  (cpu-ivr a-cpu))
(define (set-ivr! value)
  (set-cpu-ivr! a-cpu value))

(define (adr)
  (cpu-adr a-cpu))
(define (set-adr! value)
  (set-cpu-adr! a-cpu value))

(define (mdr)
  (cpu-mdr a-cpu))
(define (set-mdr! value)
  (set-cpu-mdr! a-cpu value))

(define (ir)
  (cpu-ir a-cpu))
(define (set-ir! value)
  (set-cpu-ir! a-cpu value))

(define (mar)
  (cpu-mar a-cpu))
(define (set-mar! value)
  (set-cpu-mar! a-cpu value))

(define mir
  (lambda ([from 0]
           [to (string-length (cpu-mir a-cpu))])
    (substring (cpu-mir a-cpu) from to)))
(define (set-mir! value)
  (set-cpu-mir! a-cpu value))

(define (memory address)
  (vector-ref (cpu-memory a-cpu) address))
(define (memory-range start stop)
  (vector-copy (cpu-memory a-cpu) start stop))
(define (set-memory! address value)
  (vector-set! (cpu-memory a-cpu) address value))
(define (memory-copy list start)
  (for ([i (vector-length list)]
        [item list])
    (set-memory! (+ i start) item)))

(define (microprogram address)
  (vector-ref (cpu-microprogram a-cpu) address))
(define (set-microprogram! microprogram)
  (set-cpu-microprogram! a-cpu microprogram))

(define (state)
  (cpu-state a-cpu))
(define (set-state! state)
  (set-cpu-state! a-cpu state))

(define (sbus)
  (cpu-sbus a-cpu))
(define (set-sbus! value)
  (set-cpu-sbus! a-cpu value))

(define (dbus)
  (cpu-dbus a-cpu))
(define (set-dbus! value)
  (set-cpu-dbus! a-cpu value))

(define (rbus)
  (cpu-rbus a-cpu))
(define (set-rbus! value)
  (set-cpu-rbus! a-cpu value))

(define (sum)
  (println "sum")
  (let ([sum (bin (+ (dec (sbus)) (dec (dbus))) 16)])
    (set-rbus! sum)))

(define (pdiroffs)
  (println "pdiroffs")
  (set-sbus! (bin (dec (substring (ir) 8 16)))))

(define (pd0s)
  (println "pd0s")
  (set-sbus! (bin 0 16)))

(define (pdiroffd)
  (println "pdiroffd")
  (set-dbus! (bin (dec (substring (ir) 8 16)))))

(define (pdpcd)
  (println "pdpcd")
  (set-dbus! (pc)))

(define (pmadr)
  (println "pmadr")
  (set-adr! (rbus)))

(define (+2pc)
  (println "+2pc")
  (let ([new-pc (bin (+ (dec (pc)) 1) 16)])
    (set-pc! new-pc)))

(define (none)
  (println "none"))

(define (ifch)
  (println "ifch")
  (let ([ir (memory (dec (adr)))])
    (set-ir! ir)))

(define (read)
  (println "read")
  (set-mdr! (memory (dec (adr)))))

(define (write)
  (println "write")
  (set-memory! (dec (adr)) (mdr)))

(define (error op block)
  (let ([error (string-append op " " block " function not yet implemented")])
    (raise error)))

(define (pdflags)
  (println "pdflags")
  (set-sbus! (flag)))

(define (pdsps)
  (println "pdsps")
  (set-sbus! (sp)))

(define (pdts)
  (println "pdts")
  (set-sbus! (t)))

(define (pdnotts)
  (println "pdnotts")
  (set-sbus! (bitstring-not (t))))

(define (pdpcs)
  (println "pdpcs")
  (set-sbus! (pc)))

(define (pdivrs)
  (println "pdivrs")
  (set-sbus! (ivr)))

(define (pdadrs)
  (println "pdadrs")
  (set-sbus! (adr)))

(define (pdmdrs)
  (println "pdmdrs")
  (set-sbus! (mdr)))

(define (pdrgs)
  (println "pdrgs")
  (set-sbus! (register (dec (substring (ir) 6 10)))))

(define (pd-1s)
  (println "pd-1s")
  (set-sbus! (bitstring-not (bin 0))))

(define (pd1s)
  (println "pd1s")
  (set-sbus! (bin 1)))

(define (exec-sbus op)
  (match op
    ["0000" (none)]
    ["0001" (pdiroffs)]
    ["0010" (pdflags)]
    ["0011" (pdsps)]
    ["0100" (pdts)]
    ["0101" (pdnotts)]
    ["0110" (pdpcs)]
    ["0111" (pdivrs)]
    ["1000" (pdadrs)]
    ["1001" (pdmdrs)]
    ["1010" (pdrgs)]
    ["1011" (pd0s)]
    ["1100" (pd-1s)]
    ["1101" (pd1s)]
    [else (error op "sbus")]))

(define (pdflagd)
  (println "pdflagd")
  (set-dbus! (flag)))

(define (pdspd)
  (println "pdspd")
  (set-dbus! (sp)))

(define (pdtd)
  (println "pdtd")
  (set-dbus! (t)))

(define (pdnottd)
  (println "pdnottd")
  (set-dbus! (bitstring-not (t))))

(define (pdmdrd)
  (println "pdmdrd")
  (set-dbus! (mdr)))

(define (pdivrd)
  (println "pdivrd")
  (set-dbus! (ivr)))

(define (pdadrd)
  (println "pdadrd")
  (set-dbus! (adr)))

(define (pdrgd)
  (println "pdrgd")
  (set-dbus! (register (dec (substring (ir) 6 10)))))

(define (pd0d)
  (println "pd0d")
  (set-dbus! (bin 0)))

(define (exec-dbus op)
  (match op
    ["0000" (none)]
    ["0001" (pdiroffd)]
    ["0010" (pdflagd)]
    ["0011" (pdspd)]
    ["0100" (pdtd)]
    ["0101" (pdnottd)]
    ["0110" (pdpcd)]
    ["0111" (pdivrd)]
    ["1000" (pdadrd)]
    ["1001" (pdmdrd)]
    ["1010" (pdrgd)]
    ["1011" (pd0d)]
    [else (error op "dbus")]))

(define (exec-alu op)
  (match op
    ["0000" (none)]
    ["0001" (sum)]
    [else (error op "alu")]))

(define (pmflag)
  (println "pmflag")
  (set-flag! (rbus)))

(define (pmrg)
  (println "pmrg")
  (set-register! (dec (substring (ir) 12 16)) (rbus)))

(define (pmsp)
  (println "pmsp")
  (set-sp! (rbus)))

(define (pmpc)
  (println "pmpc")
  (set-pc! (rbus)))

(define (pmivr)
  (println "pmivr")
  (set-ivr! (rbus)))

(define (pmt)
  (println "pmt")
  (set-t! (rbus)))

(define (pmmdr)
  (println "pmmdr")
  (set-mdr! (rbus)))

(define (exec-rbus op)
  (match op
    ["0000" (none)]
    ["0001" (pmflag)]
    ["0010" (pmrg)]
    ["0011" (pmsp)]
    ["0100" (pmt)]
    ["0101" (pmpc)]
    ["0110" (pmivr)]
    ["0111" (pmadr)]
    ["1000" (pmmdr)]
    [else (error op "rbus")]))

(define (string-replace-index string index value)
    (let ([v (list->vector (string->list string))])
      (vector-set! v index value)
      (list->string (vector->list v))))

(define (+2sp)
  (println "+2sp")
  (set-sp! (bin (+ (dec (sp)) 1))))

(define (-2sp)
  (println "-2sp")
  (set-sp! (bin (- (dec (sp)) 1))))

(define (a0c)
  (println "a0c")
  (set-flag! (string-replace-index (flag) 2 #\0)))

(define (a1c)
  (println "a1c")
  (set-flag! (string-replace-index (flag) 2 #\1)))

(define (a0v)
  (println "a0v")
  (set-flag! (string-replace-index (flag) 5 #\0)))

(define (a1v)
  (println "a1v")
  (set-flag! (string-replace-index (flag) 5 #\1)))

(define (a0z)
  (println "a0z")
  (set-flag! (string-replace-index (flag) 3 #\0)))

(define (a1z)
  (println "a1z")
  (set-flag! (string-replace-index (flag) 3 #\1)))

(define (a0s)
  (println "a0s")
  (set-flag! (string-replace-index (flag) 4 #\0)))

(define (a1s)
  (println "a1s")
  (set-flag! (string-replace-index (flag) 4 #\1)))

(define (a0bvi)
  (println "a0bvi")
  (set-flag! (string-replace-index (flag) 9 #\0)))

(define (a1bvi)
  (println "a1bvi")
  (set-flag! (string-replace-index (flag) 9 #\1)))

(define (exec-other op)
  (match op
    ["00000" (none)]
    ["00001" (begin (println "pdcond not implemented"))]
    ["00010" (begin (println "cinpdcond not implemented"))]
    ["00011" (+2sp)]
    ["00100" (-2sp)]
    ["00101" (+2pc)]
    ["00111" (a0c)]
    ["01000" (a1c)]
    ["01001" (a0v)]
    ["01010" (a1v)]
    ["01011" (a0z)]
    ["01100" (a1z)]
    ["01101" (a0s)]
    ["01110" (a1s)]
    ["01111" (begin (a0c) (a0v) (a0z) (a0s))]
    ["10000" (begin (a1c) (a1v) (a1z) (a1s))]
    ["10001" (a0bvi)]
    ["10010" (a1bvi)]
    [else (error op "other")]))

(define (exec-mem op)
  (match op
    ["00" (none)]
    ["01" (ifch)]
    ["10" (read)]
    ["11" (write)]
    [else (error op "memory")]))

(define (c)
  (println "c")
  (if (string=? (substring (flag) 2 3) "1") #t #f))

(define (z)
  (println "z")
  (if (string=? (substring (flag) 3 4) "1") #t #f))

(define (s)
  (println "s")
  (if (string=? (substring (flag) 4 5) "1") #t #f))

(define (v)
  (println "v")
  (if (string=? (substring (flag) 5 6) "1") #t #f))

(define (ad)
  (println "ad")
  (if (string=? (substring (ir) 10 12) "01") #t #f))

(define (aclow)
  (println "aclow")
  (if (string=? (substring (flag) 7 8) "1") #t #f))

(define (cil)
  (println "cil")
  (if (string=? (substring (flag) 8 9) "1") #t #f))

(define (f)
  (let ([op (mir 48 52)])
    (match op
      ["0000" #t]
      ["0001" #f]
      ["0010" (c)]
      ["0011" (z)]
      ["0100" (s)]
      ["0101" (v)]
      ["0110" (ad)]
      ["0111" (aclow)]
      ["1000" (cil)]
      [else (error op "f")])))

(define (g)
  (println "g")
  (let ([ntf (mir 55 56)])
    (let ([ntf (string=? ntf "1")])
      (xor ntf (f)))))

(define (cl0)
  (let ([ir15 (string->number (substring (ir) 0 1))]
        [ir14 (bitwise-bit-field (bitwise-not (dec (ir))) 14 15)])
    (bitwise-and ir15 (bitwise-not ir14))))

(define (cl1)
  (let ([ir15 (string->number (substring (ir) 0 1))]
        [ir13 (string->number (substring (ir) 2 3))])
    (bitwise-and ir15 ir13)))

(define (cl-bitwise)
  (println "cl")
  (let ([cl0 (cl0)]
        [cl1 (cl1)])
    (bitwise-ior (arithmetic-shift cl1 1) cl0)))

(define (index1)
  (println "index1")
  (let ([op1 (substring (ir) 0 1)]
        [op2 (substring (ir) 1 3)])
    (cond
      [(string=? op1 "0") 0]
      [(string=? op2 "00") 1]
      [(string=? op2 "01") 2]
      [(string=? op2 "11") 3]
      [else (error (substring (ir) 0 3) "cl")])))

(define (index2)
  (println "index2")
  (let ([op (substring (ir) 4 6)])
    (* (string->number op 2) 2)))

(define (index3)
  (println "index3")
  (let ([op (substring (ir) 10 12)])
    (* (string->number op 2) 2)))

(define (index4)
  (println "index4")
  (let ([op (substring (ir) 1 4)])
    (* (string->number op 2) 2)))

(define (index5)
  (println "index5")
  (let ([op (substring (ir) 3 7)])
    (* (string->number op 2) 2)))

(define (index6)
  (println "index6")
  (let ([op (substring (ir) 3 7)])
    (* (string->number op 2) 2)))

(define (index7)
  (println "index7")
  (let ([op (substring (ir) 3 8)])
    (* (string->number op 2) 2)))

(define (index)
  (let ([index (mir 52 55)])
    (match index
      ["000" 0]
      ["001" (index1)]
      ["010" (index2)]
      ["011" (index3)]
      ["100" (index4)]
      ["101" (index5)]
      ["110" (index6)]
      ["111" (index7)]
      [else (error index "index")])))

(define (step)
  (match (state)
    [0 (begin
         (set-mir! (microprogram (dec (mar))))
         (set-state! 1))]
    [1 (begin
         (let ([opcode (mir 25 29)])
           (exec-sbus opcode))
         (set-state! 2))]
    [2 (begin
         (let ([opcode (mir 29 33)])
           (exec-dbus opcode))
         (set-state! 3))]
    [3 (begin
         (let ([opcode (mir 33 37)])
           (exec-alu opcode))
         (set-state! 4))]
    [4 (begin
         (let ([opcode (mir 37 41)])
           (exec-rbus opcode))
         (set-state! 5))]
    [5 (begin
         (let ([opcode (mir 41 46)])
           (exec-other opcode))
         (set-state! 6))]
    [6 (begin
         (let ([opcode (mir 46 48)])
           (exec-mem opcode))
         (set-state! 7))]
    [7 (if (g)
           (set-state! 8)
           (set-state! 9))]
    [8 (let ([index (index)]
             [offset (dec (mir 56 64))])
         (let ([new-mar (bin (+ index offset))])
           (set-mar! new-mar)
           (set-state! 0)))]
    [9 (let ([new-mar (bin (+ (dec (mar)) 1) 16)])
         (set-mar! new-mar)
         (set-state! 0))]))

(define a-cpu (make-cpu))
