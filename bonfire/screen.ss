#lang scheme/gui

(require "mark.ss"
         "plot.ss")

(define width 300)
(define height 300)
(define padding 20)

(define (plot-screen frame #:title [title "Bonfire"])
  (define f (new frame%
               [label title]
               [width (inexact->exact (ceiling (+ (* 2 padding) height)))]
               [height (inexact->exact (ceiling (+ (* 2 padding) width)))]))
  (define c (new (class canvas%
                   (super-new)
                   (define/override (on-event evt)
                     (when (send evt button-down?)
                       (printf "[~a ~a]\n" (send evt get-x) (send evt get-y)))))
                 [parent f]
                 [paint-callback (make-plotter frame)]))
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
  ;; (Channelof 'forward 'backward 'current)
  (define t-chan (make-channel))
  ;; (Channelof Frame)
  (define c-chan (make-channel))
  (define t
    (let* ([frames (list->vector frames)]
           [n (vector-length frames)])
      (thread
       (lambda ()
         (let loop ([idx 0] [saved null])
           (match (channel-get t-chan)
                  ['forward
                   (loop (modulo (add1 idx) n) saved)]
                  ['backward
                   (loop (modulo (sub1 idx) n) saved)]
                  ['current
                   (printf "Displaying slide ~a\n" idx)
                   (channel-put c-chan (vector-ref frames idx))
                   (loop idx saved)]
                  ['save
                   (if (member idx saved)
                       (loop idx saved)
                       (loop idx (cons idx saved)))]
                  ['dump
                   (printf "Saved slides:\n~a\n" saved)
                   (loop idx saved)]))))))
  (define c (new (class canvas%
                   (super-new)
                   (define (forward)
                     (channel-put t-chan 'forward)
                     (send this refresh)
                     (yield))
                   (define (backward)
                     (channel-put t-chan 'backward)
                     (send this refresh)
                     (yield))
                   (define (save)
                     (channel-put t-chan 'save)
                     (yield))
                   (define (dump)
                     (channel-put t-chan 'dump)
                     (yield))
                   (define/override (on-char evt)
                     (case (send evt get-key-code)
                       [(left up) (backward)]
                       [(right down) (forward)]
                       [(#\space) (save)]
                       [(#\return) (dump)]))
                   (define/override (on-event evt)
                     (cond
                      [(send evt button-down? 'left)
                       (forward)]
                      [(send evt button-down? 'right)
                       (backward)])))
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