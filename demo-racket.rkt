#lang racket

(require threading)

#|

1. repl - int
2. repl - string
3. repl - function call

(string->number "1234321")

4. repl - images

(require 2htdp/image)
(let sierpinski ([n 8])
  (cond
    [(zero? n) (triangle 2 'solid 'red)]
    [else (define t (sierpinski (- n 1)))
          (freeze (above t (beside t t)))]))

5. repl - 3d

(require pict3d)
(adaptive-deform
   (with-color (rgba "turquoise")
     (ellipsoid (pos 0 -1/2 0) (dir 1/4 1/4 1)))
   (twist 360))

|#

#|

1. macro - normal call

(number->string (add1 (add1 (add1 (add1 (string->number "14"))))))

2. macro - threading

(~> "14" string->number add1 add1 add1 add1 number->string)

|#
