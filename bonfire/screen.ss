#lang scheme/gui

(require "mark.ss"
         "plot.ss")

(define width 300)
(define height 300)
(define padding 20)

(define (plot-screen label box)
  (define f (new frame%
               [label label]
               [width (inexact->exact (ceiling (+ (* 2 padding) height)))]
               [height (inexact->exact (ceiling (+ (* 2 padding) width)))]))
  (define c (new (class canvas%
                   (super-new)
                   (define/override (on-event evt)
                     (when (send evt button-down?)
                       (printf "[~a ~a]\n" (send evt get-x) (send evt get-y)))))
                 [parent f]
                 [paint-callback (make-plotter box)]))
  (send f show #t)
  f)

(provide
 plot-screen)