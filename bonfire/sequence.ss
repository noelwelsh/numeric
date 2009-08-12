#lang scheme/base

(require (prefix-in ra: (planet dvanhorn/ralist:1)))

(define (map-sequence fn s)
  (ra:reverse
   (for/fold ([lst ra:null])
       ([x s])
     (ra:cons (fn x) lst))))
  

(provide
 in-sequence)