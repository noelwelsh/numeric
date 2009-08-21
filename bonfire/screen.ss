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

;; (Listof Frame) -> Void
;;
;; Click on the window to advance through a list of Frames
(define (slides frames #:title [title "Slides"])
  (define f (new frame%
               [label title]
               [width (inexact->exact (ceiling (+ (* 2 padding) height)))]
               [height (inexact->exact (ceiling (+ (* 2 padding) width)))]))
  ;; (Channelof 'advance 'current)
  (define t-chan (make-channel))
  ;; (Channelof Frame)
  (define c-chan (make-channel))
  (define t (thread
             (lambda ()
               (let loop ([frames frames])
                 (if (null? frames)
                     #f
                     (match (channel-get t-chan)
                       ['advance
                        (loop (cdr frames))]
                       ['current
                        (channel-put c-chan (car frames))
                        (loop frames)]))))))
  (define c (new (class canvas%
                   (super-new)
                   (define/override (on-event evt)
                     (when (send evt button-down?)
                       (channel-put t-chan 'advance)
                       (send this refresh)
                       (yield))))
                 [parent f]
                 [paint-callback
                  (lambda (canvas dc)
                    (channel-put t-chan 'current)
                    (send dc clear)
                    ((make-plotter (channel-get c-chan)) canvas dc))]))
  (send f show #t)
  f)


(provide
 plot-screen
 slides)