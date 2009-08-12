#lang scheme/base
  
(require (planet schematics/schemeunit:3/test)
         (planet williams/science:3/science)
         "dirichlet.ss"
         "vector.ss")

(provide dirichlet-tests)
  
(define dirichlet-tests
  (test-suite
   "All tests for dirichlet"

   (test-case
    "Uniform Dirichlet PDF correct"
    (let ([d (vector 1.0 1.0)])
      (check-= (dirichlet-pdf (vector 0.5 0.5) d) 1.0 0.01)
      (check-= (dirichlet-pdf (vector 0.7 0.3)d) 1.0 0.01)))

   (test-case
    "Non-uniform Dirichlet PDF correct"
    (let ([d (vector 2.0 2.0)])
      (check-= (dirichlet-pdf (vector 0.5 0.5) d) 1.5 0.01)))

   (test-case
    "Dirichlet PDF is zero for point not in simplex"
    (let ([d (vector 2.0 2.0)])
      (check-= (dirichlet-pdf (vector 0.0 0.0) d) 0.0 0.01)
      (check-= (dirichlet-pdf (vector 0.1 0.5) d) 0.0 0.01)
      (check-= (dirichlet-pdf (vector 1.0 1.2) d) 0.0 0.01)))

   (test-case
    "dirichlet-pdf raises exn on contract violation"
    (check-exn
     exn:fail:contract?
     (lambda ()
       (dirichlet-pdf (vector 0.0 1.0) (vector 1.0 1.0 1.0)))))

   (test-case
    "dirichlet-bayesian-update produces correct result"
    (let ([d (vector 1.0 1.0)])
      (check-= (dirichlet-pdf (vector 0.5 0.5) d) 1.0 0.01)
      (check-=
       (dirichlet-pdf
        (vector 0.5 0.5)
        (dirichlet-bayesian-update (vector 1.0 1.0) d))
       1.5
       0.01)))

   (test-case
    "dirichlet-bayesian-update raises exn on contract violation"
    (check-exn
     exn:fail:contract?
     (lambda ()
       (let ([d (vector 1.0 1.0)])
         (dirichlet-bayesian-update (vector 1.0) d)))))

   (test-case
    "dirichlet-pdf is numerically stable"
    (check-false (zero? (dirichlet-pdf (vector 0.99 0.005 0.005)
                                       (vector 2.0 2.0 2.0)))))

   (test-case
    "random-dirichlet"
    (for
     ([i (in-range 10)])
     (check-= (vector-sum (random-dirichlet (vector 1 2 3)))
              1.0
              0.01)))

   (test-case
    "dirichlet-sample-likelihood correct for simple case"
    (check-= (dirichlet-sample-likelihood (vector 1 0) (vector 1 1))
             (* (/ (gamma 2) (gamma 3))
                (* (/ (gamma 2) (gamma 1))
                   (/ (gamma 1) (gamma 1))))
             0.00001))

   (test-case
    "dirichlet-bayesian-retract produces correct result"
    (let* ([d (vector 2.0 2.0)]
           [d-prime (dirichlet-bayesian-retract (vector 1.0 1.0) d)])
      (check-vector= d-prime (vector 1.0 1.0) 0.0001)
      (check-= (dirichlet-pdf (vector 0.5 0.5) d-prime)
               1.0
               0.01)))
                                            
     
   ))
