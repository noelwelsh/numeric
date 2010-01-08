#lang scheme/gui

(require
 "mark.ss"
 "plot.ss"
 (except-in "frame.ss" Frame))

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
                 [paint-callback
                  (lambda (canvas dc)
                    (setup-dc-and-draw dc frame))]))
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
                    (setup-dc-and-draw dc (channel-get c-chan)))]))
  (send f show #t)
  f)


;;; Utilities

(define white-brush (make-object brush% "white" 'solid))
(define white-pen (make-object pen% "white" 1 'solid))
(define clear-brush (make-object brush% "white" 'transparent))

;; Hack so match can work with Typed Scheme structs
(define-struct Frame (offset-x offset-y left top width height style) #:omit-define-values)


;; For plotting to the screen we setup a square DC and scale
;; the plot to fit
(define (setup-dc-and-draw dc f)
  (match-define (struct Frame (ox oy l t w h s)) f)
  (define bottom (+ t h))
  (define right (+ l w))
  (define data-bounding-square-width (max w h))

  (define-values (width height) (send dc get-size))
  (define canvas-bounding-square-width (min width height))
  (define scale (/ canvas-bounding-square-width data-bounding-square-width))
  (define origin (vector-immutable ox oy))
  (printf "Scale ~a\n" scale)
  (parameterize
      ([current-scale scale])
    ;; Clearing the DC doesn't always work, so we attempt
    ;; drawing a white rectangle over it
    (send dc set-origin 0 0)
    (send dc set-scale 1 1)
    (send dc set-brush white-brush)
    (send dc set-pen white-pen)
    (send dc draw-rectangle 0 0 width height)
    
    (send dc clear)
    
    (send dc set-smoothing 'smoothed)
    (send dc set-origin (* scale (- l)) (* scale (- t)))
    (send dc set-scale scale scale)
    
    (send dc set-brush clear-brush)
    (draw-frame dc origin f)))

(provide
 plot-screen
 slides)