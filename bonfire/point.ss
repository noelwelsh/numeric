#lang typed-scheme

;; Points represented as vectors
;;
;; A point is a vector of at least two elements, both of
;; which are numbers. This representation allows the vector
;; to contain other data, which may or may not be
;; interpreted by Bonfire.

;; The primary utility for this abstraction is to allow the
;; client to provide a wider range of data types, and hence
;; do less data munging

(define-type-alias Point (Vectorof Number))

(: point-x (Point -> Number))
(define (point-x pt)
  (vector-ref pt 0))

(: point-y (Point -> Number))
(define (point-y pt)
  (vector-ref pt 1))

(: point-r (Point -> Number))
(define (point-r pt)
  (vector-ref pt 2))

(: point-w (Point -> Number))
(define (point-w pt)
  (vector-ref pt 2))

(: point-h (Point -> Number))
(define (point-h pt)
  (vector-ref pt 3))

(: point-end-x (Point -> Number))
(define (point-end-x pt)
  (vector-ref pt 2))

(: point-end-y (Point -> Number))
(define (point-end-y pt)
  (vector-ref pt 3))

(provide
 Point
 point-x
 point-y
 point-r
 point-w
 point-h
 point-end-x
 point-end-y)