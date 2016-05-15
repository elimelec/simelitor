#lang racket/gui

(require "elisembler.rkt")
(require "bin.rkt")

(define (open-file button event)
  (define path (get-file))
  (when path
    (define source (file->lines path))
    (define asembled (compile-asm path))

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

(define (open-microcode button event)
  (define path (get-file))
  (when path
    (define path-string (path->string path))
    (define microprogram-bin (file->lines path-string))
    (define microprogram-text (file->lines (string-replace path-string "bin" "txt")))

    (define source-code (new list-box%
                             [label #f]
                             [parent microcode-panel]
                             [choices microprogram-text]
                             [style (list 'single
                                          'column-headers)]
                             [columns '("Text Microcode")]))
    (define asembled-code (new list-box%
                               [label #f]
                               [parent microcode-panel]
                               [choices microprogram-bin]
                               [style (list 'single
                                            'column-headers)]
                               [columns '("Binary Microcode")]))
    source-code))

(define frame (new frame% [label "Simelitor"]))

(define buttons-panel (new horizontal-panel%
                           [parent frame]
                           [style (list 'border)]))
(define source-code-panel (new horizontal-panel%
                                   [parent frame]
                                   [style (list 'border)]))
(define microcode-panel (new horizontal-panel%
                                   [parent frame]
                                   [style (list 'border)]))

(define open-source-button (new button%
                    [parent buttons-panel]
                    [label "Open Source"]
                    [callback open-file]))
(define open-microcode-button (new button%
                    [parent buttons-panel]
                    [label "Open Microcode"]
                    [callback open-microcode]))

(send frame show #t)


