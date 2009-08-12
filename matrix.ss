;;;
;;; Time-stamp: <2009-06-03 17:23:38 noel>
;;;
;;; Copyright (C) by Noel Welsh. 
;;;

#lang scheme/base

;; Basic operators for matrices
;;
;; This code currently uses a matrix implementation derived
;; from the GSL, and hence is under the GPL, not the LGPL as
;; the rest of this library is.


(require (planet schematics/mzgsl:1/matrix)
         (planet schematics/mzgsl:1/linear-algebra)
         "for.ss"
         "vector.ss")


;; Vector/matrix operations These are not provided by
;; vector.ss as we don't want to infect vector.ss with the
;; GPL.

;; Vector/matrix functions
(define (vector*m v m)
  (define r (matrix-rows m))
  (define c (matrix-cols m))
  
  (for/vector ([j c])
    (for/sum ([i (in-range r)]
              [x (in-vector v)])
             (* x (matrix-ref m i j)))))

(define (vector+m v m)
  (error "vector+m not implemented"))

(define (vector/m v m)
  (error "vector/m not implemented"))

(define (vector-m v m)
  (error "vector-m not implemented"))
  
;; Matrix/vector functions
(define (matrix*v m v)
  (define r (matrix-rows m))
  (define c (matrix-cols m))
  
  (for/vector ([i r])
    (for/sum ([j (in-range c)]
              [x (in-vector v)])
             (* x (matrix-ref m i j)))))

(provide vector*m
         vector+m
         vector/m
         vector-m
         matrix*v
         (all-from-out (planet schematics/mzgsl:1/matrix)
                       (planet schematics/mzgsl:1/linear-algebra)))