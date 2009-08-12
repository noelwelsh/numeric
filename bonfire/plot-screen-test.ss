#lang scheme/base

;; Manual tests that rendering is correct
;;
;; Run these tests from MrEd

(require "plot.ss"
         "screen.ss"
         "frame.ss")


;; Four circles arranged in a square. This tests mark
;; constructors and frame coordinates
(define (four-circles)
  (plot-screen "Four Circles" (circles (list (vector 0 0 3) (vector 0 12 3) (vector 12 12 3) (vector 12 0 3)))))

;; A square!  Tests lines
(define (square)
  (plot-screen "Square" (lines (list (cons (vector 2 0) (vector 3 2))
                                     (cons (vector 3 2) (vector 1 3))
                                     (cons (vector 1 3) (vector 0 1))
                                     (cons (vector 0 1) (vector 2 0))))))

;; Tests crosses and colours
(define (colourful-crosses)
  (plot-screen "Colourful Crosses" (overlay
                                    (colour (crosses (list (vector 0 0) (vector 10 0) (vector 0 10) (vector 10 10)))
                                            (vector 255 0 0 1))
                                    (colour (crosses (list (vector 0 5) (vector 5 0) (vector 5 10) (vector 10 5)))
                                            (vector 0 0 255 1)))))
(provide
 (all-defined-out))