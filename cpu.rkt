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
  (let ([new-pc (bin (+ (dec (pc)) 2) 16)])
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

(define (exec-sbus op)
  (cond
    [(string=? op "0000") (none)]
    [(string=? op "1011") (pd0s)]
    [else (error op "sbus")]))

(define (exec-dbus op)
  (cond
    [(string=? op "0000") (none)]
    [(string=? op "0110") (pdpcd)]
    [else (error op "dbus")]))

(define (exec-alu op)
  (cond
    [(string=? op "0000") (none)]
    [(string=? op "0001") (sum)]
    [else (error op "alu")]))

(define (exec-rbus op)
  (cond
    [(string=? op "0000") (none)]
    [(string=? op "0111") (pmadr)]
    [else (error op "rbus")]))

(define (exec-other op)
  (cond
    [(string=? op "00000") (none)]
    [(string=? op "00101") (+2pc)]
    [else (error op "other")]))

(define (exec-mem op)
  (cond
    [(string=? op "00") (none)]
    [(string=? op "01") (ifch)]
    [(string=? op "10") (read)]
    [(string=? op "11") (write)]
    [else (error op "memory")]))

(define (f)
  (let ([op (mir 48 52)])
    (cond
      [(string=? op "0000") #t]
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

(define (index)
  (let ([index (mir 52 55)])
    (match index
      ["000" 0]
      ["001" (index1)]
      [else (error index "index")])))

(define (step)
  (cond
    [(= (state) 0) (begin
                     (set-mir! (microprogram (dec (mar))))
                     (set-state! 1))]
    [(= (state) 1) (begin
                     (let ([opcode (mir 25 29)])
                       (exec-sbus opcode))
                     (set-state! 2))]
    [(= (state) 2) (begin
                     (let ([opcode (mir 29 33)])
                       (exec-dbus opcode))
                     (set-state! 3))]
    [(= (state) 3) (begin
                     (let ([opcode (mir 33 37)])
                       (exec-alu opcode))
                     (set-state! 4))]
    [(= (state) 4) (begin
                     (let ([opcode (mir 37 41)])
                       (exec-rbus opcode))
                     (set-state! 5))]
    [(= (state) 5) (begin
                     (let ([opcode (mir 41 46)])
                       (exec-other opcode))
                     (set-state! 6))]
    [(= (state) 6) (begin
                     (let ([opcode (mir 46 48)])
                       (exec-mem opcode))
                     (set-state! 7))]
    [(= (state) 7) (if (g)
                       (set-state! 8)
                       (set-state! 9))]
    [(= (state) 8) (let ([index (index)]
                         [offset (dec (mir 56 64))])
                     (let ([new-mar (bin (+ index offset))])
                       (set-mar! new-mar)
                       (set-state! 0)))]
    [(= (state) 9) (let ([new-mar (bin (+ (dec (mar)) 1) 16)])
                     (set-mar! new-mar)
                     (set-state! 0))]))

(define a-cpu (make-cpu))
