#lang racket/gui

(require "elisembler.rkt")
(require "bin.rkt")

(define (create-list parent choices name)
  (new list-box%
       [label #f]
       [parent parent]
       [choices choices]
       [style (list 'single
                    'column-headers)]
       [columns (list name)]))

(define (open-file button event)
  (define path (get-file))
  (when path
    (create-list source-panel (file->lines path) "Source Code")
    (create-list source-panel (compile-asm path) "Asembled Code")))

(define (open-microcode button event)
  (define path (get-file))
  (when path
    (define path-string (path->string path))
    (define microprogram-bin (file->lines path-string))
    (define microprogram-text (file->lines (string-replace path-string "bin" "txt")))

    (create-list microcode-panel microprogram-text "Text Microcode")
    (create-list microcode-panel microprogram-bin "Binary Microcode")))

(define frame (new frame% [label "Simelitor"]))

(define buttons-panel (new horizontal-panel%
                           [parent frame]
                           [style (list 'border)]))
(define source-panel (new horizontal-panel%
                          [parent frame]
                          [style (list 'border)]))
(define microcode-panel (new horizontal-panel%
                             [parent frame]
                             [style (list 'border)]))
(define eval-panel (new horizontal-panel%
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

(define eval-input (new text-field%
                        [label "Code"]
                        [parent eval-panel]
                        [callback (lambda (text-field event) "Eval Text")]))
(define eval-button (new button%
                         [parent eval-panel]
                         [label "Eval"]
                         [callback (lambda (button event) "Eval")]))

(send frame show #t)