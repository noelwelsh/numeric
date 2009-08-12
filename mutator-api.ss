#lang scheme/base

(require "unet.ss")


(define-unet define-mutators
  (import vector-ref
          vector-set!)
  (export vector-add1!
          vector-sub1!)

  (define (vector-add1! v idx)
    (vector-set! v idx (add1 (vector-ref v idx))))
          
  (define (vector-sub1! v idx)
    (vector-set! v idx (sub1 (vector-ref v idx)))))
         

(provide define-mutators)