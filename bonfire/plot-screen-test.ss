#lang scheme/base

;; Manual tests that rendering is correct
;;
;; Run these tests from MrEd

(require "main.ss")


;; Four circles arranged in a square. This tests mark
;; constructors and frame coordinates
(define (four-circles)
  (plot-screen (circles
                (list (point 0 0 3)
                      (point 0 12 3)
                      (point 12 12 3)
                      (point 12 0 3)))
               #:title "Four Circles"))

;; A square!  Tests lines
(define (square)
  (plot-screen #:title "Square"
               (lines (list (cons (point 2 0) (point 3 2))
                            (cons (point 3 2) (point 1 3))
                            (cons (point 1 3) (point 0 1))
                            (cons (point 0 1) (point 2 0))))))

;; Tests crosses, colours, and widths
(define (colourful-crosses)
  (plot-screen #:title "Colourful Crosses"
               (overlay
                (width
                 (colour
                  (crosses (list (point 0 0) (point 10 0) (point 0 10) (point 10 10)))
                  (point 255 0 0 1))
                 4)
                (width
                 (colour
                  (crosses (list (point 0 5) (point 5 0) (point 5 10) (point 10 5)))
                  (point 0 0 255 1))
                 2))))

;; Slides
(define (travelling-circle)
  (slides (list
           (circles (list (point 0 0 4) (point 0 0 10)))
           (circles (list (point 1 1 4) (point 0 0 10)))
           (circles (list (point 2 2 4) (point 0 0 10)))
           (circles (list (point 3 3 4) (point 0 0 10)))
           (circles (list (point 4 4 4) (point 0 0 10))))))

(provide
 (all-defined-out))