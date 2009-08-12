#lang scheme/base

(require scheme/unit
         (planet schematics/schemeunit:3)
         "api-sig.ss")

(define-syntax make-vector-tests
  (syntax-rules ()
    [(_ vector for/fold/vector for/vector in-vector in-vector-reverse)
     (lambda (unit)
       (define-values/invoke-unit unit
         (import)
         (export vector^))

       (test-suite
        "vector-tests"

        ;; Basics
        
        (test-case
         "for/vector"
         (check-equal?
          (for/vector ([i 4]) i)
          (vector 0 1 2 3)))

        (test-case
         "for/vector many arg form"
         (let-values (([v1 v2 v3]
                       (for/vector ([i 4 3])
                                   (values i (* 2 i) (* 3 i)))))
           (check-equal?
            v1
            (vector 0 1 2 3))
           (check-equal?
            v2
            (vector 0 2 4 6))
           (check-equal?
            v3
            (vector 0 3 6 9))))

        (test-case
         "for/vector with nested sequences"
         (check-equal?
          (for/vector ([_ 16]
                       [i (in-range 4)]
                       #:when #t
                       [j (in-range 4)])
            j)
          (vector 0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3)))

        (test-case
         "for/fold/vector"
         (let-values (([lst vec]
                       (for/fold/vector ([lst null]) ([i 4])
                                        (values (cons i lst) i))))
           (check-equal? lst '(3 2 1 0))
           (check-equal? vec (vector 0 1 2 3))))

        (test-case
         "for/fold/vector many arg form"
         (let-values (([lst vec0 vec1]
                       (for/fold/vector ([lst null]) ([i 4 2])
                                        (values (cons i lst) i (- i)))))
           (check-equal? lst '(3 2 1 0))
           (check-equal? vec0 (vector 0 1 2 3))
           (check-equal? vec1 (vector 0 -1 -2 -3))))

        (test-case
         "for/fold/vector allows internal define"
         (let-values (([a b]
                       (for/fold/vector ([x 0])
                                        ([i 4])
                                        (define foo (* i i))
                                        (values foo foo))))
           (check-equal? a 9)
           (check-equal? b (vector 0 1 4 9))))

        (test-case
         "for/fold/vector with nested sequences"
         (let-values (([lst vec]
                       (for/fold/vector ([lst null])
                                        ([_ 16]
                                         [i (in-range 4)]
                                         #:when #t
                                         [j (in-range 4)])
                         (values (cons j lst) j))))
           (check-equal? lst (list   3 2 1 0 3 2 1 0 3 2 1 0 3 2 1 0))
           (check-equal? vec (vector 0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3))))
   
        (test-case
         "in-vector-reverse"
         (check-equal?
          (for/list ([v (in-vector-reverse (vector 1 2 3 4))])
                    v)
          '(4 3 2 1)))

        (test-case
         "in-vector-reverse w/ offset"
         (check-equal?
          (for/list ([v (in-vector-reverse (vector 1 2 3 4) 1)])
                    v)
          '(3 2 1)))

        (test-case
         "in-vector"
         (check-equal?
          (for/list ([v (in-vector (vector 1 2 3 4))])
                    v)
          '(1 2 3 4)))

        ;; Test error checking for comprehensions code
        (test-case
         "in-vector error handling"
         (check-exn (lambda (e)
                      (and (exn:fail:contract? e)
                           (check-regexp-match
                            #rx"expected argument of type.*given \\(1 2 3 4\\)"
                            (exn-message e))))
                    (lambda ()
                      (in-vector '(1 2 3 4)))))
        
        ;; Constructors
        
        (test-case
         "vector-ones"
         (check-equal? (vector-ones 5) (vector 1 1 1 1 1)))

        (test-case
         "vector-zeros"
         (check-equal? (vector-zeros 5) (vector 0 0 0 0 0)))

        (test-case
         "vector-copy"
         (check-equal? (vector 1 2 3 4 5) (vector-copy (vector 1 2 3 4 5)))
         (check-equal? (vector) (vector-copy (vector))))

        
        ;; Predicates

        (test-case
         "vector-null?"
         (check-true (vector-null? (vector)))
         (check-false (vector-null? (vector 1))))

        (test-case
         "vector-="
         (check-true (vector-= (vector 1 2 3 4) (vector 1 2 3 4)) "Equal vectors")
         (check-false (vector-= (vector 1 2 7 4) (vector 1 2 3 4)) "Different values")
         (check-false (vector-= (vector 1 2 3 4) (vector 1 2 3 4 5)) "Different lengths")
         (check-true (vector-= (vector 1 2 3.1 4) (vector 1 2 3 4) 0.25) "Within epsilon")
         (check-true (vector-= (vector 1 2 3.25 4) (vector 1 2 3 4) 0.25) "At epsilon")
         (check-false (vector-= (vector 1 2 3.11 4) (vector 1 2 3 4) 0.1) "Outside epsilon"))
        
        ;; Selectors

        (test-case
         "vector-select"
         (check-equal? (vector-select (vector 0 1 2 3 4 5) (vector #f #f #f #f #f #f))
                       (vector))
         (check-equal? (vector-select (vector 0 1 2 3 4 5) (vector #t #t #t #t #t #t))
                       (vector 0 1 2 3 4 5))
         (check-equal? (vector-select (vector 0 1 2 3 4 5) (vector #f #t #f #t #f #t))
                       (vector 1 3 5)))
        
        (test-case
         "vector-last"
         (check-equal? (vector-last (vector 0 1 2 7)) 7))

        (test-case
         "vector-find"
         (check-equal? (vector-find (vector 0 1 2 3 4) 2) (vector 2))
         (check-equal? (vector-find (vector 0 1 0 1 0) 0) (vector 0 0 0))
         (check-equal? (vector-find (vector 0 1 2 3 4) 5) (vector))
         (check-equal? (vector-find (vector 0 1 2 3 4) odd?) (vector 1 3))
         (check-equal? (vector-find (vector 2 4 6 8) odd?) (vector)))

        (test-case
         "vector-findi"
         (check-equal? (vector-findi (vector 0 1 2 3 4) 2) (vector 2))
         (check-equal? (vector-findi (vector 0 1 0 1 0) 0) (vector 0 2 4))
         (check-equal? (vector-findi (vector 0 1 2 3 4) 5) (vector))
         (check-equal? (vector-findi (vector 0 1 2 3 4) odd?) (vector 1 3))
         (check-equal? (vector-findi (vector 2 4 6 8) odd?) (vector)))

        (test-case
         "vector-find-first"
         (check-equal? (vector-find-first (vector 0 1 2 3 4) 2) 2)
         (check-equal? (vector-find-first (vector 0 1 0 1 0) 0) 0)
         (check-equal? (vector-find-first (vector 0 1 2 3 4) 5) #f)
         (check-equal? (vector-find-first (vector 0 1 2 3 4) odd?) 1)
         (check-equal? (vector-find-first (vector 2 4 6 8) odd?) #f))

        (test-case
         "vector-find-firsti"
         (check-equal? (vector-find-firsti (vector 0 2 0 0 0) 2) 1)
         (check-equal? (vector-find-firsti (vector 1 1 0 1 0) 0) 2)
         (check-equal? (vector-find-firsti (vector 0 1 2 3 4) 5) #f)
         (check-equal? (vector-find-firsti (vector 0 1 2 3 4) odd?) 1)
         (check-equal? (vector-find-firsti (vector 2 4 6 8) odd?) #f))

        (test-case
         "vector-remove"
         (check-equal? (vector-remove (vector 0 2 0 0 0) 2) (vector 0 0 0 0))
         (check-equal? (vector-remove (vector 1 1 0 1 0) 0) (vector 1 1 1))
         (check-equal? (vector-remove (vector 0 1 2 3 4) 5) (vector 0 1 2 3 4))
         (check-equal? (vector-remove (vector 0 1 2 3 4) odd?) (vector 0 2 4))
         (check-equal? (vector-remove (vector 2 4 6 8) odd?) (vector 2 4 6 8)))

        (test-case
         "vector-remove-first"
         (check-equal? (vector-remove-first (vector 0 2 0 0 0) 2) (vector 0 0 0 0))
         (check-equal? (vector-remove-first (vector 1 1 0 1 0) 0) (vector 1 1 1 0))
         (check-equal? (vector-remove-first (vector 0 1 2 3 4) 5) (vector 0 1 2 3 4))
         (check-equal? (vector-remove-first (vector 0 1 2 3 4) odd?) (vector 0 2 3 4))
         (check-equal? (vector-remove-first (vector 2 4 6 8) odd?) (vector 2 4 6 8)))

        (test-case
         "vector-removei"
         (check-equal? (vector-removei (vector 0 1 2 3 4) 2) (vector 0 1 3 4))
         (check-equal? (vector-removei (vector 0 1 2 3 4) 0) (vector 1 2 3 4))
         (check-equal? (vector-removei (vector 0 1 2 3 4) 4) (vector 0 1 2 3)))

        (test-case
         "vector-slice"
         (check-equal? (vector-slice (vector 0 1 2 3 4) 0) (vector))
         (check-equal? (vector-slice (vector 0 1 2 3 4) 3) (vector 0 1 2))
         (check-equal? (vector-slice (vector 0 1 2 3 4) 5) (vector 0 1 2 3 4))
         (check-equal? (vector-slice (vector 0 1 2 3 4) 0 5) (vector 0 1 2 3 4))
         (check-equal? (vector-slice (vector 0 1 2 3 4) 1 3) (vector 1 2))
         (check-equal? (vector-slice (vector 0 1 2 3 4) 1 2) (vector 1))
         (check-equal? (vector-slice (vector 0 1 2 3 4) 1 1) (vector)))
        
        ;; Mutators
   
        (test-case
         "vector-add1!"
         (let ([v (vector 0 1 2 3)])
           (vector-add1! v 1)
           (check-equal? v (vector 0 2 2 3))))

        (test-case
         "vector-sub1!"
         (let ([v (vector 0 1 2 3)])
           (vector-sub1! v 1)
           (check-equal? v (vector 0 0 2 3))))


        ;; General Functions

        (test-case
         "vector/scalar"
         (let ([v (vector 0 2 4 6)])
           (check-equal? (vector*s v 2) (vector 0 4 8 12) "vector*s")
           (check-equal? (vector/s v 2) (vector 0 1 2 3)  "vector/s")
           (check-equal? (vector-s v 2) (vector -2 0 2 4) "vector-s")
           (check-equal? (vector+s v 2) (vector 2 4 6 8)  "vector+s")))
   
        (test-case
         "vector-sum"
         (let ([v (vector 0 1 2 3)])
           (check-equal? (vector-sum v) 6)))

        (test-case
         "vector-adjoin"
         (check-equal? (vector-adjoin (vector 1 2 3 4) 5) (vector 1 2 3 4 5))
         (check-equal? (vector-adjoin (vector) 1) (vector 1))
         (check-equal? (vector-adjoin (vector 1 2 3) (vector 4))
                       (vector 1 2 3 (vector 4))))

        (test-case
         "vector-append"
         (check-equal? (vector-append (vector 1 2 3 4) (vector 5 6 7 8))
                       (vector 1 2 3 4 5 6 7 8))
         (check-equal? (vector-append (vector) (vector)) (vector))
         (check-equal? (vector-append (vector) (vector 1 2 3 4)) (vector 1 2 3 4))
         (check-equal? (vector-append (vector 1 2 3 4) (vector)) (vector 1 2 3 4)))

        (test-case
         "vector-append varg case"
         (check-equal? (vector-append) (vector))
         (check-equal? (vector-append (vector 1 2 3)) (vector 1 2 3))
         (check-equal? (vector-append (vector 1) (vector 2) (vector 3) (vector 4))
                       (vector 1 2 3 4)))
        
        (test-case
         "vector-max"
         (check-equal? (vector-max (vector 0 1 2 3 4 5)) 5)
         (check-equal? (vector-max (vector 5 4 3 2 1 0)) 5)
         (check-equal? (vector-max (vector 4 3 2 5 1 0)) 5)
         (check-equal? (vector-max (vector 5 5 5 5 5 5)) 5)
         (check-equal? (vector-max (vector -5 -4 -3 -2 -1 0)) 0))

        (test-case
         "vector-min"
         (check-equal? (vector-min (vector 0 1 2 3 4 5)) 0)
         (check-equal? (vector-min (vector 5 4 3 2 1 0)) 0)
         (check-equal? (vector-min (vector 4 3 2 5 1 0)) 0)
         (check-equal? (vector-min (vector 5 5 5 5 5 5)) 5)
         (check-equal? (vector-min (vector -5 -4 -3 -2 -1 0)) -5))
        
        ;; Iteration

        (test-case
         "vector-map"
         (check-equal? (vector-map - (vector 1 2 3 4)) (vector -1 -2 -3 -4))
         (check-equal? (vector-map + (vector 1 2 3 4) (vector 1 2 3 4))
                       (vector 2 4 6 8)))

        (test-case
         "vector-map checks length of arguments"
         (check-exn exn:fail:contract?
                    (lambda () (vector-map + (vector) (vector 1 2)))))
        
        (test-case
         "vector-mapi"
         (check-equal? (vector-mapi (lambda (i x) i) (vector 4 3 2 1)) (vector 0 1 2 3)))
        
        (test-case
         "vector-fold"
         (check-= (vector-fold (lambda (x seed) (+ x seed)) 4 (vector 0 1 2 3))
                  10.0
                  0.0))
        
        (test-case
         "vector-foldi"
         (check-= (vector-foldi (lambda (i x seed) (+ x i seed)) 4 (vector 1 2 3 4))
                  20.0
                  0.0))

        ))]))

(provide make-vector-tests)