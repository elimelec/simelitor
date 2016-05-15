#lang racket/gui

(require "elisembler.rkt")
(require "bin.rkt")

(define (open-file button event)
  (define path (get-file))
  (when path
    (define source (file->lines path))
    (define asembled (compile-asm path))

    (define source-code-panel (new horizontal-panel%
                                   [parent frame]
                                   [style (list 'border)]))
    (define source-code (new list-box%
                             [label #f]
                             [parent source-code-panel]
                             [choices source]
                             [style (list 'single
                                          'column-headers)]
                             [columns '("Source code")]))
    (define asembled-code (new list-box%
                               [label #f]
                               [parent source-code-panel]
                               [choices asembled]
                               [style (list 'single
                                            'column-headers)]
                               [columns '("Asembled code")]))
    source-code))

(define frame (new frame% [label "Simelitor"]))

(define button (new button%
                    [parent frame]
                    [label "Open"]
                    [callback open-file]))

(send frame show #t)


