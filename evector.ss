#lang scheme/base

(require (only-in "evector/evector.ss"
                  make-evector evector evector?
                  evector-ref evector-set! evector-length
                  list->evector)
         "unet.ss"
         "comprehensions-api.ss"
         "constructor-api.ss"
         "predicate-api.ss"
         "selector-api.ss"
         "function-api.ss"
         "mutator-api.ss"
         "iteration-api.ss")

;;; Comprehensions

(define-comprehensions
  (import "evector"
          'in-evector
          make-evector
          evector?
          evector-ref
          evector-set!
          evector-length)
  (export for/evector
          for/fold/evector
          in-evector
          in-evector-reverse))

;;; Constructors

(define-constructors
  (import make-evector
          for/evector
          in-evector
          evector-length)
  (export evector-ones
          evector-zeros
          evector-copy))

;;; Predicates

(define-predicates
  (import in-evector evector-length)
  (export evector-null? evector-=))

;;; Selectors

(define-selectors
  (import for/evector for/fold/evector in-evector evector-ref evector-length list->evector)
  (export evector-select
          evector-last
          evector-find evector-findi
          evector-find-first evector-find-firsti
          evector-slice))

;;; Mutators

(define-mutators
  (import evector-ref evector-set!)
  (export evector-add1! evector-sub1!))

;;; Iterators

(define-iterators 
  (import for/fold/evector for/evector
          in-evector
          evector-length)
  (export evector-map evector-mapi
          evector-fold evector-foldi))

;;; General Functions

(define-functions
  (import for/evector
          in-evector
          evector-set!
          evector-ref
          evector-length
          evector-find-firsti)
  (export evector* evector+ evector/ evector- 
          evector*s evector/s evector+s evector-s 
          evector-sum evector-product
          evector-dot
          evector-normalise
          evector-max evector-maxi evector-min evector-mini
          evector-adjoin evector-append
          evector-remove evector-remove-first evector-removei))

(provide make-evector evector evector?
         evector-ref evector-set! evector-length
         list->evector
         (all-defined-out))