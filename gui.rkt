#lang racket/gui

(require "elisembler.rkt")
(require "cpu.rkt")
(require "bin.rkt")

(define (repeat f count)
  (for ([i (in-range count)])
    (f)))

(define-syntax-rule (while test body ...)
  (let loop () (when test body ... (loop))))

(define (repeat-until test f)
  (while test (f)))

(define perform-step
  (lambda ([update-gui #f])
    (step)
    (when update-gui (update-lists))))

(define (registers-list-values)
  (list (flag) (sp) (t) (pc) (ivr) (adr) (ir)
        (mar) (mir) (number->string (state)) (sbus) (dbus) (rbus)))
(define (registers-list-names)
  (list "flag" "sp" "t" "pc" "ivr" "adr" "ir"
        "mar" "mir" "state" "sbus" "dbus" "rbus"))

(define (update-lists)
  (send registers-list set (vector->list (registers)))
  (send memory-list set (vector->list (memory-range 0 65536)))
  (send cpu-registers-list-names set (registers-list-names))
  (send cpu-registers-list-values set (registers-list-values)))

(define (create-list parent choices name)
  (new list-box%
       [label #f]
       [parent parent]
       [choices choices]
       [style (list 'single
                    'column-headers)]
       [columns (list name)]))

(define source-code-list #f)
(define source-code-assembled #f)
(define (load-source-code path)
  (let ([source (file->lines path)]
        [assembly (compile-asm-file path)])
    (set! source-code-list (create-list source-panel source "Source Code"))
    (set! source-code-assembled (create-list source-panel assembly "Assembled Code"))
    (memory-copy (list->vector assembly) 0)))

(define microprogram-bin-list #f)
(define microprogram-text-list #f)
(define (load-microcode path)
  (define path-string (path->string path))
  (define microprogram-bin (file->lines path-string))
  (define microprogram-text (file->lines (string-replace path-string "bin" "txt")))

  (set! microprogram-text-list (create-list microcode-panel microprogram-text "Text Microcode"))
  (set! microprogram-bin-list (create-list microcode-panel microprogram-bin "Binary Microcode"))
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

(define root-panel (new horizontal-panel%
                        [parent frame]
                        [style (list 'border)]))
(define left-panel (new vertical-panel%
                        [parent root-panel]
                        [style (list 'border)]))
(define rigth-panel (new vertical-panel%
                         [parent root-panel]
                         [style (list 'border)]))

(define buttons-panel (new horizontal-panel%
                           [parent left-panel]
                           [style (list 'border)]))
(define source-panel (new horizontal-panel%
                          [parent left-panel]
                          [style (list 'border)]))
(define microcode-panel (new horizontal-panel%
                             [parent left-panel]
                             [style (list 'border)]))
(define eval-panel (new horizontal-panel%
                        [parent left-panel]
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
                         [callback (lambda (button event) (perform-step #t))]))

(define step-i-button (new button%
                           [parent buttons-panel]
                           [label "Step Instruction"]
                           [callback (lambda (button event)
                                       (repeat perform-step 8)
                                       (perform-step #t))]))

(define run-button (new button%
                        [parent buttons-panel]
                        [label "Run"]
                        [callback (lambda (button event)
                                    (repeat perform-step 3000)
                                    (perform-step #t))]))

(define registers-list (create-list
                        rigth-panel
                        (vector->list (registers))
                        "Registers"))

(define memory-list (create-list
                     rigth-panel
                     (vector->list (memory-range 0 65536))
                     "Memory"))

(define registers-panel (new horizontal-panel%
                        [parent rigth-panel]
                        [style (list 'border)]))
(define cpu-registers-list-names (create-list
                              registers-panel
                              (registers-list-names)
                              "Registers"))
(define cpu-registers-list-values (create-list
                               registers-panel
                               (registers-list-values)
                               "Values"))

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
  (set-memory! 0 (first code))
  (update-lists))
(define eval-input (new text-field%
                        [label "Code"]
                        [parent eval-panel]
                        [callback eval-input-changed]))
(define eval-button (new button%
                         [parent eval-panel]
                         [label "Eval"]
                         [callback (lambda (button event) (eval-asm))]))

(send frame show #t)
(load-source-code (string->path "test.s"))
(load-microcode (string->path "microprogram.bin"))
(update-lists)
