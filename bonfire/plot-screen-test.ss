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
  (plot-screen "Four Circles"
               (circles
                (list (vector-immutable 0 0 3)
                      (vector-immutable 0 12 3)
                      (vector-immutable 12 12 3)
                      (vector-immutable 12 0 3)))))

;; A square!  Tests lines
(define (square)
  (plot-screen "Square"
               (lines (list (cons (vector-immutable 2 0) (vector-immutable 3 2))
                            (cons (vector-immutable 3 2) (vector-immutable 1 3))
                            (cons (vector-immutable 1 3) (vector-immutable 0 1))
                            (cons (vector-immutable 0 1) (vector-immutable 2 0))))))

;; Tests crosses and colours
(define (colourful-crosses)
  (plot-screen "Colourful Crosses"
               (overlay
                (colour (crosses (list (vector-immutable 0 0) (vector-immutable 10 0) (vector-immutable 0 10) (vector-immutable 10 10)))
                        (vector-immutable 255 0 0 1))
                (colour (crosses (list (vector-immutable 0 5) (vector-immutable 5 0) (vector-immutable 5 10) (vector-immutable 10 5)))
                        (vector-immutable 0 0 255 1)))))

;; Slides
(define (travelling-circle)
  (slides (list
           (circles (list (vector-immutable 0 0 4) (vector-immutable 0 0 10)))
           (circles (list (vector-immutable 1 1 4) (vector-immutable 0 0 10)))
           (circles (list (vector-immutable 2 2 4) (vector-immutable 0 0 10)))
           (circles (list (vector-immutable 3 3 4) (vector-immutable 0 0 10)))
           (circles (list (vector-immutable 4 4 4) (vector-immutable 0 0 10))))))

(provide
 (all-defined-out))