#lang scheme/base

(require
 (planet schematics/schemeunit:3)
 "util.ss")

(define/provide-test-suite util-tests
  (test-case
   "in-2d-range"
   (check-equal?
    (for/list ([(i j) (in-2d-range (5 5))])
      (list i j))
    '((0 0) (0 1) (0 2) (0 3) (0 4)
      (1 0) (1 1) (1 2) (1 3) (1 4)
      (2 0) (2 1) (2 2) (2 3) (2 4)
      (3 0) (3 1) (3 2) (3 3) (3 4)
      (4 0) (4 1) (4 2) (4 3) (4 4)))
   (check-equal?
    (for/list ([(i j) (in-2d-range (1 1) (5 5))])
      (list i j))
    '((1 1) (1 2) (1 3) (1 4)
      (2 1) (2 2) (2 3) (2 4)
      (3 1) (3 2) (3 3) (3 4)
      (4 1) (4 2) (4 3) (4 4)))
   (check-equal?
    (for/list ([(i j) (in-2d-range (1 1) (5 5) (2 2))])
      (list i j))
    '((1 1) (1 3)
      (2 1) (2 3)
      (3 1) (3 3)))))