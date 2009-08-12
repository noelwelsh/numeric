#lang scheme/base

(require "unet.ss")


(define-unet define-iterators
  (import for/fold/vector
          for/vector
          in-vector
          vector-length)
  (export vector-map vector-mapi
          vector-fold vector-foldi)

  (define vector-map
    (case-lambda
      [(fn v)
       (for/vector ([i (vector-length v)]
                    [x (in-vector v)])
                   (fn x))]
      [(fn v1 v2)
       (for/vector ([i (vector-length v1)]
                    [x1 (in-vector v1)]
                    [x2 (in-vector v2)])
                   (fn x1 x2))]))
  
  (define (vector-mapi fn v)
    (for/vector ([i (vector-length v)]
                 [x (in-vector v)])
                (fn i x)))
  
  (define (vector-fold fn seed v)
    (for/fold ([seed seed])
              ([x (in-vector v)])
              (fn x seed)))
  
  (define (vector-foldi fn seed v)
    (for/fold ([seed seed])
              ([i (in-range (vector-length v))]
               [x (in-vector v)])
              (fn i x seed))))

(provide define-iterators)