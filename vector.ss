#lang scheme/base

(require (for-syntax scheme/base)
         "unet.ss"
         "comprehensions-api.ss"
         "constructor-api.ss"
         "predicate-api.ss"
         "selector-api.ss"
         "function-api.ss"
         "mutator-api.ss"
         "iteration-api.ss"
         "check-api.ss")

(require scheme/function)

;;; Comprehensions and Sequences

(define-comprehensions
  (import 'vector
          'in-vector
          make-vector
          vector?
          vector-ref
          vector-set!
          vector-length)
  (export for/vector
          for/fold/vector
          _
          in-vector-reverse))

;;; Constructors

(define-constructors 
  (import make-vector
          for/vector
          in-vector
          vector-length)
  (export vector-ones
          vector-zeros
          vector-copy
          vector-reverse))

;;; Predicates

(define-predicates
  (import in-vector vector-length)
  (export vector-null? vector-=))

;;; Selectors

(define-selectors
  (import for/vector for/fold/vector in-vector vector-ref vector-length list->vector)
  (export vector-select
          vector-last
          vector-find vector-findi
          vector-find-first vector-find-firsti
          vector-slice))

;;; Mutators

(define-mutators
  (import vector-ref vector-set!)
  (export vector-add1! vector-sub1!))

;;; Iterators

(define-iterators 
  (import for/fold/vector for/vector
          in-vector
          vector-length)
  (export vector-map vector-mapi
          vector-fold vector-foldi))

;;; General Functions

(define-functions
  (import for/vector
          in-vector
          vector-set!
          vector-ref
          vector-length
          vector-find-firsti)
  (export vector* vector+ vector/ vector- 
          vector*s vector/s vector+s vector-s 
          vector-sum vector-product
          vector-dot
          vector-normalise
          vector-max vector-maxi vector-min vector-mini
          vector-adjoin vector-append
          vector-remove vector-remove-first vector-removei))

;;; Checks

(define-checks
  (import in-vector
          vector-length)
  (export check-vector=))

(provide (all-defined-out))