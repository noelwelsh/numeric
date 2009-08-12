#lang scheme/base
  
(require (planet schematics/schemeunit:3/test))
(require "multinomial.ss")
  
(provide multinomial-tests)

(define multinomial-tests
  (test-suite
   "All tests for multinomial"

   (test-case
    "random-multinomial samples from simple distributions"
    (check-equal? (random-multinomial '((a . 1.0))) 'a)
    (check-equal? (random-multinomial '((a . 0.0) (b . 1.0))) 'b)
    (check-equal? (random-multinomial '((a . 1.0) (b . 0.0))) 'a))

   (test-case
    "random-multinomial samples from complex distributions"
    (let* ([the-multinomial '((a . 0.2) (b . 0.4) (c . 0.2) (d . 0.2))]
           [sample-it
            (lambda (random-number)
              (random-multinomial the-multinomial (lambda () random-number)))])

      (check-equal? (sample-it 0.1) 'a)
      (check-equal? (sample-it 0.3) 'b)
      (check-equal? (sample-it 0.5) 'b)
      (check-equal? (sample-it 0.7) 'c)
      (check-equal? (sample-it 0.9) 'd)
      (check-equal? (sample-it 1.0) 'd)
      (check-equal? (sample-it 0.0) 'a)))

   (test-case
    "random-multinomial works with any dict type"
    (let* ([the-multinomial #(0.2 0.4 0.2 0.2)]
           [sample-it
            (lambda (random-number)
              (random-multinomial the-multinomial (lambda () random-number)))])
      (check-equal? (sample-it 0.1) 0)
      (check-equal? (sample-it 0.3) 1)
      (check-equal? (sample-it 0.5) 1)
      (check-equal? (sample-it 0.7) 2)
      (check-equal? (sample-it 0.9) 3)
      (check-equal? (sample-it 1.0) 3)
      (check-equal? (sample-it 0.0) 0)))
     
   ))
  
