#lang scheme/gui

(require
 "mark.ss"
 "plot.ss"
 (except-in "frame.ss" Frame))

(define (plot-file file-name frame)
  (define setup (new ps-setup%))
  (send setup set-file file-name)
  (send setup set-mode 'file)
  (parameterize ([current-ps-setup setup])
    (define dc (new post-script-dc% [interactive #f]))
    (send dc start-doc file-name)
    (send dc start-page)
    (setup-dc-and-draw dc frame)
    (send dc end-page)
    (send dc end-doc)))


;;; Utilities

(define clear-brush (make-object brush% "white" 'transparent))

;; Hack so match can work with Typed Scheme structs
(define-struct Frame (offset-x offset-y left top width height style) #:omit-define-values)

;; Scale somewhat
(define (setup-dc-and-draw dc f)
  (match-define (struct Frame (ox oy l t w h s)) f)
  (define bottom (+ t h))
  (define right (+ l w))
  (define data-bounding-square-width (max w h))

  (define-values (width height) (send dc get-size))
  (define canvas-bounding-square-width (min width height))
  (define scale (/ canvas-bounding-square-width data-bounding-square-width))
  (define origin (vector-immutable ox oy))
  (parameterize
      ([current-scale scale])
    (send dc set-smoothing 'smoothed)
    (send dc set-origin (* scale (- l)) (* scale (- t)))
    (send dc set-scale scale scale)
    
    (send dc set-brush clear-brush)
    (draw-frame dc origin f)))

(provide
 plot-file)