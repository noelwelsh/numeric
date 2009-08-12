#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "multivariate-gaussian.ss"
         "matrix.ss"
         "vector.ss")

(define mu (vector 1 1))
(define sigma (matrix 2 2  2 1 1 2))
(define cholesky (matrix-cholesky sigma))
;; (listof (cons point prob))
;;
;; This test data comes from the Matlab mvnpdf function
;;
;; E.g.
;; mvnpdf([1 1], [1 1], [2 1; 1 2])
;;
;; ans =
;;
;;    0.0919
(define test-data
  '((#(1 1) . 0.0919)
    (#(2 2) . 0.0658)
    (#(1.5 1.5) . 0.0845)
    (#(3 3) . 0.0242)))
    
(define e 0.0001)

(define/provide-test-suite multivariate-gaussian-tests

  (test-case
   "random-multivariate-gaussian"
   (for ([i (in-range 10)])
     (let ([s (random-multivariate-gaussian mu cholesky)])
       (check-equal? (vector-length s) 2))))
  
  (test-case
   "multivariate-gaussian-pdf"
   (for-each
    (lambda (pt)
      (define x (car pt))
      (define p (cdr pt))
      (check-= (multivariate-gaussian-pdf x mu cholesky) p e))
    test-data))

  (test-case
   "make-multivariate-gaussian"
   (define-values (sampler pdf)
     (make-multivariate-gaussian mu sigma))
   
   (for-each
    (lambda (pt)
      (define x (car pt))
      (define p (cdr pt))
      (check-= (pdf x) p e))
    test-data)

   (for ([i (in-range 10)])
     (let ([s (sampler)])
       (check-equal? (vector-length s) 2))))
  )