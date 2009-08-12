;;;
;;; Time-stamp: <2009-07-22 11:19:48 noel>
;;;
;;; Copyright (C) by Noel Welsh. 
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

;; Basic operators for f64vectors
;;
;; Types:
;;   vector : synonym for f64vector
;;
;; Naming convention
;;   v prefix indicates operates on vector
;;   / suffix indicates a fold
;;   . suffix indicates operates element-wise
#lang scheme/base

(require scheme/foreign
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
  (import "f64vector"
          'in-f64vector
          make-f64vector
          f64vector?
          f64vector-ref
          f64vector-set!
          f64vector-length)
  (export for/f64vector
          for/fold/f64vector
          in-f64vector
          in-f64vector-reverse))

;;; Constructors

(define-constructors
  (import make-f64vector
          for/f64vector
          in-f64vector
          f64vector-length)
  (export f64vector-ones
          f64vector-zeros
          f64vector-copy))

;;; Predicates

(define-predicates
  (import in-f64vector f64vector-length)
  (export f64vector-null? f64vector-=))

;;; Selectors

(define-selectors
  (import for/f64vector for/fold/f64vector in-f64vector f64vector-ref f64vector-length list->f64vector)
  (export f64vector-select
          f64vector-last
          f64vector-find f64vector-findi
          f64vector-find-first f64vector-find-firsti
          f64vector-slice))

;;; Mutators

(define-mutators
  (import f64vector-ref f64vector-set!)
  (export f64vector-add1! f64vector-sub1!))

;;; Iterators

(define-iterators 
  (import for/fold/f64vector for/f64vector
          in-f64vector
          f64vector-length)
  (export f64vector-map f64vector-mapi
          f64vector-fold f64vector-foldi))

;;; General Functions

(define-functions
  (import for/f64vector
          in-f64vector
          f64vector-set!
          f64vector-ref
          f64vector-length
          f64vector-find-firsti)
  (export f64vector* f64vector+ f64vector/ f64vector- 
          f64vector*s f64vector/s f64vector+s f64vector-s 
          f64vector-sum f64vector-product
          f64vector-dot
          f64vector-normalise
          f64vector-max f64vector-maxi f64vector-min f64vector-mini
          f64vector-adjoin f64vector-append
          f64vector-remove f64vector-remove-first f64vector-removei))

(provide (all-defined-out)
         make-f64vector
         f64vector
         f64vector?
         f64vector-ref
         f64vector-set!
         f64vector-length)
