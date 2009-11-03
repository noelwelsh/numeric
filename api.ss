#lang scheme/base

(require (for-syntax scheme/base))

(require scheme/unit
         srfi/26/cut
         (planet untyped/unlib:3/syntax))

;; Basic: these must be provided by the user
;; make-vector
;; vector
;; vector?
;; vector-length
;; vector-set!
;; vector-ref
(define base-names
  '(-length -set! -ref))

;; Comprehensions
;; for/fold/vector
;; for/vector
;; in-vector
(define (comprehesion-ids stx prefix)
  (list (make-id stx 'for/fold/ prefix)
        (make-id stx 'for/ prefix)
        (make-id stx 'in- prefix)))

;; Constructors
;; vector-ones
;; vector-zeros
;; vector-copy
(define constructor-names
  '(-ones -zeros -copy -reverse))

;; Predicates
;; vector-null?
;; vector-=
(define predicate-names
  '(-null? -=))

;; Display
;; vector->string
;; display-vector

;; Selectors
;; vector-last
;; vector-find vector-findi
;; vector-find-first vector-find-firsti
;; vector-slice
(define selector-names
  '(-select -last -find -findi -find-first -find-firsti -slice))

;; Mutation
;; vector-add1!
;; vector-sub1!
(define mutator-names
  '(-add1! -sub1!))

;; Iteration
;; vector-map
;; vector-mapi
;; vector-fold
;; vector-foldi
(define iterator-names
  '(-map -mapi -fold -foldi))

;; General
;;  Vector/vector element-wise
;; vector*
;; vector+
;; vector/
;; vector-
;;  Vector/scalar
;; vector*s
;; vector+s
;; vector/s
;; vector-s
;;  Vector reduction
;; vector-sum
;; vector-product
;;  Vector/vector other
;; vector-dot
;;  Other
;; vector-normalise
;; vector-max
;; vector-maxi
;; vector-min
;; vector-mini
;; vector-adjoin
;; vector-append
;; vector-remove
;; vector-remove-first
;; vector-removei
(define function-names
  '(*  /  +  -
    *s /s +s -s
    -sum -product
    -dot
    -normalise
    -max -maxi
    -min -mini
    -adjoin
    -append
    -remove
    -remove-first
    -removei))


(define names
  (append constructor-names predicate-names selector-names mutator-names iterator-names function-names))

(define (names->ids names stx prefix)
  (map (cut make-id stx prefix <>)
       names))


(provide (all-defined-out))