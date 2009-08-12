#lang scheme/base
  
(require
 (planet schematics/schemeunit:3)
 "f64vector.ss")
  
(define/provide-test-suite f64vector-tests
  (test-case
   "constructor v handles exact and inexact numbers"
   (check v= (v 1 2 3 4) (v 1. 2. 3. 4.)))

  (test-case
   "vadd1!"
   (let ([vec (v 1 2 3 4)])
     (vadd1! vec 1)
     (check v=  vec (v 1 3 3 4))))
     
  (test-case
   "v+/"
   (check = (v+/ (v 1.0 1.0 1.0 1.0 1.0)) 5.0)
   (check = (v+/ (v -1 -2 -3 1 2 3)) 0.0))

  (test-case
   "v+s"
   (check v= (v+s (v 1 2 3 4) 1)  (v 2 3 4 5))
   (check v= (v+s (v 4 3 2 1) -1) (v 3 2 1 0)))
     
  (test-case
   "v-s"
   (check v= (v-s (v 1 2 3 4) 1)  (v 0 1 2 3))
   (check v= (v-s (v 4 3 2 1) -1) (v 5 4 3 2)))

  (test-case
   "vmap"
   (check v= (vmap - (v 1 2 3 4)) (v -1 -2 -3 -4)))

  (test-case
   "vmapi"
   (check v= (vmapi (lambda (i x) i) (v 4 3 2 1)) (v 0 1 2 3)))

  (test-case
   "vfold"
   (check-equal? (vfold (lambda (x seed) (+ x seed)) 4 (v 0 1 2 3))
                 10.0))

  (test-case
   "vfoldi"
   (check-equal? (vfoldi (lambda (i x seed) (+ x i seed)) 4 (v 1 2 3 4))
                 20.0))
     
  (test-case
   "v*."
   (check v= (v*. (v 1 1 1 1) (v 1 1 1 1)) (v 1 1 1 1))
   (check v= (v*. (v 1 2 3 4) (v 4 3 2 1)) (v 4 6 6 4)))

  (test-case
   "v*. checks vectors of same size"
   (check-exn
    exn:fail:contract?
    (lambda ()
      (v*. (v 1 2 3 4) (v 1 2 3 4 5)))))

  (test-case
   "vones"
   (check v= (vones 5) (v 1 1 1 1 1)))

  (test-case
   "vnormalise"
   (check v= (vnormalise (v 1 2 3 4)) (v 0.1 0.2 0.3 0.4))) 
     
  (test-case
   "vmax"
   (check-equal? (vmax (v)) -inf.0)
   (check-= (vmax (v 1 2 3 4)) 4 0.00001)
   (check-= (vmax (v 4 3 2 1)) 4 0.00001)
   (check-= (vmax (v 3 4 2 1)) 4 0.00001))

  (test-case
   "vmax"
   (check-eq? (vmaxi (v)) -1)
   (check-eq? (vmaxi (v 1 2 3 4)) 3)
   (check-eq? (vmaxi (v 4 3 2 1)) 0)
   (check-eq? (vmaxi (v 3 4 2 1)) 1))

  (test-case
   "for/f64vector"
   (check v= (for/f64vector ([i 4]) i) (v 0 1 2 3)))

  (test-case
   "for/f64vector many arg form"
   (let-values (([v1 v2 v3]
                 (for/f64vector ([i 4 3])
                                (values i (* 2 i) (* 3 i)))))
     (check v= v1 (v 0 1 2 3))
     (check v= v2 (v 0 2 4 6))
     (check v= v3 (v 0 3 6 9))))

  (test-case
   "for/fold/f64vector"
   (let-values (([lst vec]
                 (for/fold/f64vector ([lst null]) ([i 4])
                                     (values (cons i lst) i))))
     (check-equal? lst '(3 2 1 0))
     (check v= vec (v 0 1 2 3))))

  (test-case
   "for/fold/f64vector many arg form"
   (let-values (([lst vec0 vec1]
                 (for/fold/f64vector ([lst null]) ([i 4 2])
                                     (values (cons i lst) i (- i)))))
     (check-equal? lst '(3 2 1 0))
     (check v= vec0 (v 0 1 2 3))
     (check v= vec1 (v 0 -1 -2 -3))))

  (test-case
   "in-f64vector"
   (check-equal?
    (for/list ([i (in-f64vector (v 0 1 2 3))])
              (* i 2))
    '(0. 2. 4. 6.)))

  (test-case
   "in-f64vector-reverse"
   (check-equal?
    (for/list ([i (in-f64vector-reverse (v 0 1 2 3))])
              (* i 2))
    '(6. 4. 2. 0.)))

  (test-case
   "for/fold/f64vector allows internal define"
   (let-values (([a b]
                 (for/fold/f64vector ([x 0])
                                     ([i 4])
                                     (define foo (* i i))
                                     (values foo foo))))
     (check-equal? a 9)
     (check v= b (v 0. 1. 4. 9.))))
  )

