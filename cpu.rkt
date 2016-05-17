#lang racket

(require "bin.rkt")

(provide cpu make-cpu)

(struct cpu (registers
             flag sp t pc ivr adr mdr ir
             memory
             sbus dbus rbus))

(define (make-cpu)
  (define zero (fill-bin 0 16))
  (cpu (make-vector 16 zero)
       zero zero zero zero zero zero zero zero
       (make-vector 65536 zero)
       zero zero zero))
