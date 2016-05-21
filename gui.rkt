#lang racket/gui

(require "elisembler.rkt")
(require "cpu.rkt")
(require "bin.rkt")

(define (repeat f count)
  (for ([i (in-range count)])
    (f)))

(define (create-list parent choices name)
  (new list-box%
       [label #f]
       [parent parent]
       [choices choices]
       [style (list 'single
                    'column-headers)]
       [columns (list name)]))

(define (load-source-code path)
  (let ([source (file->lines path)]
        [assembly (compile-asm-file path)])
    (create-list source-panel source "Source Code")
    (create-list source-panel assembly "Assembled Code")
    (memory-copy (list->vector assembly) 0)))

(define (load-microcode path)
  (define path-string (path->string path))
  (define microprogram-bin (file->lines path-string))
  (define microprogram-text (file->lines (string-replace path-string "bin" "txt")))

  (create-list microcode-panel microprogram-text "Text Microcode")
  (create-list microcode-panel microprogram-bin "Binary Microcode")
  (set-microprogram! (list->vector microprogram-bin)))

(define (open-file button event)
  (define path (get-file))
  (when path
    (load-source-code path)))

(define (open-microcode button event)
  (define path (get-file))
  (when path
    (load-microcode path)))

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
(define step-button (new button%
                         [parent buttons-panel]
                         [label "Step"]
                         [callback (lambda (button event) (step))]))

(define step-i-button (new button%
                           [parent buttons-panel]
                           [label "Step Instruction"]
                           [callback (lambda (button event) (repeat step 9))]))

(define (eval-input-changed text-field event)
  (let* ([event (send event get-event-type)]
         [text (send text-field get-value)]
         [ok (syntax-ok? (list text))])
    (define (set-color color) (send text-field set-field-background (make-object color% color)))
    (cond
      [(eq? event 'text-field-enter) (eval-asm)]
      [(string=? text "") (set-color "white")]
      [ok (set-color "green")]
      [else (set-color "red")])))
(define (eval-asm)
  (define input (send eval-input get-value))
  (define code (compile-asm (list input)))
  (send (send eval-input get-editor) erase)
  (set-memory! 0 (first code)))
(define eval-input (new text-field%
                        [label "Code"]
                        [parent eval-panel]
                        [callback eval-input-changed]))
(define eval-button (new button%
                         [parent eval-panel]
                         [label "Eval"]
                         [callback (lambda (button event) (eval-asm))]))

(send frame show #t)
