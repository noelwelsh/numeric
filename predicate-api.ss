#lang scheme/base

(require "unet.ss")


(define-unet define-predicates
  (import in-vector vector-length)
  (export vector-null? vector-=)

  (define (vector-null? v)
    (zero? (vector-length v)))

  (define (vector-= v1 v2 [epsilon 0])
    (and (= (vector-length v1) (vector-length v2))
         (for/and ([x1 (in-vector v1)]
                   [x2 (in-vector v2)])
           (let ([difference (abs (- x1 x2))])
             (<=  difference epsilon)))))
  )


(provide define-predicates)