#lang racket/gui

(require threading)
(require racket/vector)
(require racket/sequence)
(require (only-in racket/base (read racket-read)))

(require "elisembler.rkt")
(require "cpu.rkt")
(require "bin.rkt")

(struct breakpoint (line code enabled) #:mutable #:transparent)
(define breakpoints-list '())
(define (find-breakpoint id)
    (findf (lambda (breakpoint)
             (= (breakpoint-line breakpoint) id))
           breakpoints-list))
(define set-breakpoint
  (lambda (line [code "nop"] [enabled #t])
    (let ([breakpoint (breakpoint line code enabled)])
      (set! breakpoints-list (cons breakpoint breakpoints-list)))))
(define (disable-breakpoint id)
  (let ([breakpoint (find-breakpoint id)])
    (when breakpoint (set-breakpoint-enabled! breakpoint #f))))
(define (eval-breakpoint breakpoint)
  (set-breakpoint-enabled! breakpoint #f)
  (eval-asm (breakpoint-code breakpoint)))

(define cpu-history '())
(define cpu-history-stack '())

(define (copy-cpu a-cpu)
  (let* ([cpu-copy (struct-copy cpu a-cpu)]
         [registers-copy (vector-copy (cpu-registers cpu-copy))]
         [memory-copy (vector-copy (cpu-memory cpu-copy))])
    (set-cpu-registers! cpu-copy registers-copy)
    (set-cpu-memory! cpu-copy memory-copy)
    cpu-copy))

(define (save-cpu)
  (set! cpu-history (cons (copy-cpu a-cpu) cpu-history)))
(define (push-cpu)
  (set! cpu-history-stack (cons (copy-cpu a-cpu) cpu-history-stack)))
(define (restore-cpu)
  (when (> (length cpu-history) 1)
    (replace-cpu! (car cpu-history))
    (set! cpu-history (cdr cpu-history))))
(define (pop-cpu)
  (when (> (length cpu-history-stack) 1)
  (save-cpu)
  (replace-cpu! (car cpu-history-stack))
  (set! cpu-history-stack (cdr cpu-history-stack))))

(save-cpu)
(push-cpu)


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
        (mar) (mir) (bin (state))
        (bin (counter)) (sbus) (dbus) (rbus)))
(define (registers-list-names)
  (list "flag" "sp" "t" "pc" "ivr" "adr" "ir"
        "mar" "mir" "state"
        "counter" "sbus" "dbus" "rbus"))

(define (update-lists)
  (let* ([bin (vector->list (registers))]
         [hex (map hex bin)]
         [dec (map number->string (map sdec bin))]
         [i (map number->string (sequence->list (in-range (length bin))))])
    (send registers-list set i bin hex dec))

  (let* ([b (vector->list (memory-range 0 1024))]
         [h (map hex b)]
         [d (map (compose1 number->string sdec) b)]
         [i (~> b length in-range sequence->list (map number->string _))])
    (send memory-list set i b h d))

  (let* ([n (registers-list-names)]
         [v (registers-list-values)]
         [d (map (compose1 number->string sdec) v)]
         [h (map hex v)])
    (send other-registers-panel set n v h d))

  (let ([pc (dec (pc))]
        [first (send source-code-list get-first-visible-item)]
        [visible (sub1 (send source-code-list number-of-visible-items))])
    (send source-code-list select pc)
    (when (or (< (+ first visible) pc) (< pc first))
      (send source-code-list set-first-visible-item pc)))

  (let ([mar (dec (mar))]
        [first (send microprogram-list get-first-visible-item)]
        [visible (sub1 (send microprogram-list number-of-visible-items))])
    (send microprogram-list select mar)
    (when (or (< (+ first visible) mar) (< mar first))
      (send microprogram-list set-first-visible-item mar))))

(define (create-list parent choices name)
  (new list-box%
       [label #f]
       [parent parent]
       [choices choices]
       [style (list 'single
                    'column-headers)]
       [columns (list name)]))


(define (load-source-code path)
  (let* ([source (let ([source (file->lines "test.s")])
                   (let ([ls (~> source
                                 (map (lambda (i) (compile-asm (list i))) _)
                                 (map length _))])
                     (flatten (for/list ([instr source]
                                         [l ls])
                                (match l
                                  [1 (list instr)]
                                  [2 (list instr "")]
                                  [3 (list instr "" "")]
                                  [4 (list instr "" "" "")])))))]
         [assembly (compile-asm-file path)]
         [numbers (map number->string (sequence->list (in-range (length source))))])
    (send source-code-list set numbers source assembly)
    (memory-copy (list->vector assembly) 0)))

(define (load-microcode path)
  (define path-string (path->string path))
  (define microprogram-bin (file->lines path-string))
  (define microprogram-text (file->lines (string-replace path-string "bin" "txt")))
  (define numbers (map number->string (sequence->list (in-range (length microprogram-text)))))
  (send microprogram-list set numbers microprogram-text microprogram-bin)
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
                           [style (list 'border)]
                           [alignment (list 'center 'center)]
                           [min-height 50]
                           [stretchable-height #f]))
(define source-panel (new horizontal-panel%
                          [parent left-panel]
                          [style (list 'border)]))
(define microcode-panel (new horizontal-panel%
                             [parent left-panel]
                             [style (list 'border)]))
(define eval-panel (new horizontal-panel%
                        [parent left-panel]
                        [style (list 'border)]
                        [alignment (list 'center 'center)]
                        [min-height 50]
                        [stretchable-height #f]))

(define eval-command-panel (new horizontal-panel%
                                [parent left-panel]
                                [style (list 'border)]
                                [alignment (list 'center 'center)]
                                [min-height 50]
                                [stretchable-height #f]))

(define source-code-list
  (new list-box%
       [label #f]
       [parent source-panel]
       [choices '()]
       [style (list 'single 'column-headers)]
       [columns (list "Line" "Text" "Assembled")]))

(define microprogram-list
  (new list-box%
       [label #f]
       [parent microcode-panel]
       [choices '()]
       [style (list 'single 'column-headers)]
       [columns (list "Line" "Text" "Binary")]))

(new button%
     [parent buttons-panel]
     [label "Step"]
     [callback (lambda (button event) (save-cpu) (perform-step #t))])

(new button%
     [parent buttons-panel]
     [label "Step Micro Instruction"]
     [callback (lambda (button event)
                 (save-cpu)
                 (repeat perform-step 8)
                 (perform-step #t))])

(new button%
     [parent buttons-panel]
     [label "Step Instruction"]
     [callback (lambda (button event)
                 (save-cpu)
                 (repeat perform-step 9)
                 (while (not (= (dec (mar)) 0))
                        (perform-step))
                 (update-lists))])

(new button%
     [parent buttons-panel]
     [label "Run"]
     [callback (lambda (button event)
                 (define (continue?)
                   (let ([pc (dec (pc))])
                     (~> (memory-range pc (+ pc 4)) vector->list (map dec _) (ormap positive? _))))
                 (define (breakpoint?)
                   (let* ([pc (dec (pc))]
                          [b (find-breakpoint pc)])
                     (if b (if (breakpoint-enabled b)
                               (begin (eval-breakpoint b)
                                      #f) #t) #t)))
                 (save-cpu)
                 (while (and (continue?) (breakpoint?))
                        (perform-step))
                 (update-lists))])

(define (back-callback item event)
  (restore-cpu)
  (update-lists))

(new button%
     [parent buttons-panel]
     [label "Back"]
     [callback back-callback])

(define (push-callback item event)
  (push-cpu))

(define (pop-callback item event)
  (pop-cpu)
  (update-lists))

(new button%
     [parent buttons-panel]
     [label "Push"]
     [callback push-callback])

(new button%
     [parent buttons-panel]
     [label "Pop"]
     [callback pop-callback])

(define registers-list
  (new list-box%
       [label #f]
       [parent rigth-panel]
       [choices '()]
       [style (list 'single 'column-headers)]
       [columns (list "Register" "Binary" "Hex" "Decimal")]))

(define memory-list
  (new list-box%
       [label #f]
       [parent rigth-panel]
       [choices '()]
       [style (list 'single 'column-headers)]
       [columns (list "Address" "Binary" "Hex" "Decimal")]))

(define other-registers-panel
  (new list-box%
       [label #f]
       [parent rigth-panel]
       [choices '()]
       [style (list 'single 'column-headers)]
       [columns (list "Name" "Binary" "Hex" "Decimal")]))

(define (eval-input-changed text-field event)
  (let* ([event (send event get-event-type)]
         [text (send text-field get-value)]
         [ok (syntax-ok? (list text))])
    (define (set-color color) (send text-field set-field-background (make-object color% color)))
    (cond
      [(eq? event 'text-field-enter) (eval-asm (send eval-input get-value))]
      [(string=? text "") (set-color "white")]
      [ok (set-color "green")]
      [else (set-color "red")])))

(define (eval-asm asm)
  (let ([assembled (compile-asm (list asm))]
        [old-cpu (copy-cpu a-cpu)])
    (send (send eval-input get-editor) erase)
    (save-cpu)
    (save-cpu)
    (memory-copy (list->vector assembled) 0)
    (set-pc! (bin 0))
    (set-state! 0)
    (set-mar! (bin 0))
    (repeat perform-step 9)
    (while (not (= (dec (mar)) 0))
           (perform-step))
    (let ([new-registers (vector-copy (registers) 0)])
      (restore-cpu)
      (set-registers! new-registers))
    (update-lists)))

(define eval-input (new text-field%
                        [label "Code"]
                        [parent eval-panel]
                        [callback eval-input-changed]))
(define eval-button (new button%
                         [parent eval-panel]
                         [label "Eval"]
                         [callback (lambda (button event) (eval-asm (send eval-input get-value)))]))



(define (eval-command-input-changed text-field event)
  (let ([event (send event get-event-type)]
        [text (send text-field get-value)])
    (when (eq? event 'text-field-enter)
      (eval-command text)
      (send (send text-field get-editor) erase))))

(define (eval-command command)
  (let ([command (string-append "(" command ")")])
    (let ([result (eval (call-with-input-string command racket-read))])
      (send eval-command-result set-value
            (cond
              [(string? result) result]
              [(number? result) (number->string result)]
              [else ""])))))

(define eval-command-input (new text-field%
                                [label "Command"]
                                [parent eval-command-panel]
                                [callback eval-command-input-changed]))
(define eval-command-button (new button%
                                 [parent eval-command-panel]
                                 [label "Eval Command"]
                                 [callback (lambda (button event) (eval-asm))]))
(define eval-command-result (new text-field%
                                [label "Last Result"]
                                [parent eval-command-panel]
                                [callback eval-command-input-changed]))


(let ([menu-bar (new menu-bar% [parent frame])])
  (let ([file-menu
         (new menu% [label "File"] [parent menu-bar])])
    (new menu-item%
         [label "Open Source"]
         [parent file-menu]
         [shortcut #\o]
         [callback open-file])
    (new menu-item%
         [label "Open Microcode"]
         [shortcut #\o] [shortcut-prefix (list 'shift 'cmd)]
         [parent file-menu]
         [callback open-microcode])
    (void))
  (let ([history
         (new menu% [label "History"] [parent menu-bar])])
    (new menu-item%
         [label "Back"]
         [parent history]
         [shortcut #\z]
         [callback back-callback])
    (new menu-item%
         [label "Push State"]
         [parent history]
         [shortcut #\s]
         [callback push-callback])
    (new menu-item%
         [label "Pop State"]
         [shortcut #\s] [shortcut-prefix (list 'shift 'cmd)]
         [parent history]
         [callback pop-callback])
    (void))
  (void))

(send frame show #t)
(load-source-code (string->path "test.s"))
(load-microcode (string->path "microprogram.bin"))
(update-lists)
