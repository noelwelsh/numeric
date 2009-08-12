#lang scheme/base

(require (planet schematics/schemeunit:3/test))

(require "sparse.ss")

(define sparse-tests
  (test-suite
   "sparse-tests"

   (test-case
    "sparse removes zero elements"
    (check-equal?
     (sparse '((1 . 0) (2 . 1) (3 . 4)))
     (sparse '((2 . 1) (3 . 4)))))
   
   (test-case
    "for/sparse"
    (check-equal?
     (for/sparse ([i (in-range 4)]) (values i i))
     (sparse '((1 . 1) (2 . 2) (3 . 3)))))

   (test-case
    "smap"
    (check-equal?
     (smap (lambda (val) (- val)) (sparse '((1 . 1) (2 . 2) (3 . 3))))
     (sparse '((1 . -1) (2 . -2) (3 . -3))))
    (check-equal?
     (smap (lambda (val) 0) (sparse '((1 . 1) (2 . 2) (3 . 3))))
     (sparse null)))

   (test-case
    "smapi"
    (check-equal?
     (smapi (lambda (idx val) (- idx)) (sparse '((1 . 2) (2 . 4) (3 . 6))))
     (sparse '((1 . -1) (2 . -2) (3 . -3))))
    (check-equal?
     (smapi (lambda (idx val) 0) (sparse '((1 . 1) (2 . 2) (3 . 3))))
     (sparse null)))

   (test-case
    "sadd!"
    (let ([s (sparse '((1 . 1) (2 . 2) (3 . 3)))])
      (sadd! s 1 4)
      (sadd! s 10 0)
      (check-equal?
       s
       (sparse '((1 . 5) (2 . 2) (3 . 3))))))
   
   (test-case
    "s/."
    (check-equal?
     (s/. (sparse '((1 . 2) (2 . 4) (3 . 6)))
          (sparse '((1 . 2) (2 . 4) (3 . 6) (4 . 6))))
     (sparse '((1 . 1) (2 . 1) (3 . 1)))))

   (test-case
    "s*."
    (check-equal?
     (s*. (sparse '((1 . 2) (2 . 4) (3 . 6) (4 . 4))) (sparse '((1 . 2) (2 . 4) (3 . 6) (5 . 5))))
     (sparse '((1 . 4) (2 . 16) (3 . 36)))))

   (test-case
    "s/+"
    (check-equal?
     (s+/ (sparse '((1 . 2) (2 . 4) (3 . 6))))
     12))
   ))

(provide sparse-tests)