#lang scheme/base

(require
 scheme/dict)
  
(provide random-multinomial)

;; random-multinomial : multinomial [random-source] -> any
(define (random-multinomial m [random-source random])
  (define choice (random-source))
  ;;(printf "random-multinomial: ~a\n" m)
  (let/ec esc
    (for/fold ([sum 0.0])
              (([v p] (in-dict m)))
      (let ([new-sum (+ sum p)])
        (if (<= choice new-sum)
            (esc v)
            new-sum)))))

