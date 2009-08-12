#lang scheme/base

(require (planet schematics/schemeunit:3/test))
(require "for.ss")

(define/provide-test-suite for-tests
  (test-case
   "for/sum"
   (check-equal?
    (for/sum ([i (in-range 5)]) i)
    10))
  )