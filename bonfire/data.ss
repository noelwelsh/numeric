#lang scheme/base

(require "sequence.ss")

(define (make-point x y) (vector x y))
(define (point-x p) (vector-ref p 0))
(define (point-y p) (vector-ref p 1))

;; (sequenceof point) -> (vector point point)
(define (bounding-box pts)
  (define xs
    (for/list ([p (in-sequence pts)]) (point-x p)))
  (define ys
    (for/list ([p (in-sequence pts)]) (point-y p)))
  (define min-x (apply min xs))
  (define max-x (apply max xs))
  (define min-y (apply min ys))
  (define max-y (apply max ys))

  (vector (make-point min-x min-y) (make-point max-x max-y)))


(provide
 (all-defined-out))