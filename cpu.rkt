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
       (make-vector 138 (bin 0 64)) "00"
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

(define (mir)
  (cpu-mir a-cpu))
(define (set-mir! value)
  (set-cpu-mir! a-cpu value))

(define (memory address)
  (vector-ref (cpu-memory a-cpu) address))
(define (set-memory! address value)
  (vector-set! (cpu-memory a-cpu) address value))

(define (microprogram address)
  (vector-ref (cpu-microprogram a-cpu) address))
(define (set-microprogram! microprogram)
  (set-cpu-microprogram! a-cpu microprogram))

(define (state)
  (cpu-state a-cpu))
(define (state0?)
  (string=? (state) "00"))
(define (state1?)
  (string=? (state) "01"))
(define (state2?)
  (string=? (state) "11"))
(define (set-state! state)
  (set-cpu-state! a-cpu state))
(define (set-state0!)
  (set-state! "00"))
(define (set-state1!)
  (set-state! "01"))
(define (set-state2!)
  (set-state! "11"))

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
  (let ([sum (bin (+ (dec (sbus)) (dec (dbus))) 16)])
    (set-rbus! sum)))

(define (step)
  (cond
    [(state0?) (begin
                  (set-mir! (microprogram (dec (mar))))
                  (set-state1!))]))

(define a-cpu (make-cpu))
