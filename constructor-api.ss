#lang scheme/base

(require "unet.ss")


(define-unet define-constructors
  (import make-vector
          for/vector
          in-vector
          vector-length)
  (export vector-ones
          vector-zeros
          vector-copy)
          
  (define (vector-ones length)
    (make-vector length 1))
  
  (define (vector-zeros length)
    (make-vector length 0))
  
  (define (vector-copy v)
    (for/vector ([i (vector-length v)]
                 [x (in-vector v)])
                x)))
         

(provide define-constructors)