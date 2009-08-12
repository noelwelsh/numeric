;;;
;;; Time-stamp: <2009-05-26 11:48:58 nhw>
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
#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "for-test.ss"
         "vector-unit.ss"
         "evector-unit.ss"
         "vector.ss"
         "evector.ss"
         ;;"f64vector-test.ss"
         "vector-test.ss"
         "sparse-test.ss"
         ;;"matrix-test.ss"
         "dirichlet-test.ss"
         "multivariate-gaussian-test.ss"
         "multinomial-test.ss"
         ;;"unet-test.ss"
         ;;"unet-util-test.ss"
         ;;"util-test.ss"
         ;;"evector/evector-test.ss"
         )

(define vector-tests
  ((make-vector-tests vector for/fold/vector for/vector in-vector in-vector-reverse) vector@))

(define evector-tests
  ((make-vector-tests evector for/fold/evector for/evector in-evector in-evector-reverse) evector@))

(provide all-numeric-tests)

(define all-numeric-tests
  (test-suite 
   "all-numeric-tests"
   for-tests
   ;;f64vector-tests
   vector-tests
   sparse-tests
   ;matrix-tests
   dirichlet-tests
   multivariate-gaussian-tests
   multinomial-tests
   ;;unet-tests
   ;;unet-util-tests
   ;;util-tests
   evector-tests
   ))

