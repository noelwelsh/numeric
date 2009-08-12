#lang scheme/gui

(define commands (box (lambda (c dc)
                        ;(send dc draw-line 0 0 200 0)
                        (send dc set-scale 10 10)
                        (send dc set-origin 200 200)
                        (send dc draw-line -20 -20 20 20)
                        (send dc draw-line -20 20 20 -20))))

(define f (new frame%
               [label "DC Test"]
               [width 200]
               [height 200]))
(define c (new canvas%
               [parent f]
               [paint-callback (lambda (c dc) ((unbox commands) c dc))]))
(send f show #t)