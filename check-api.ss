#lang scheme/base

(require (planet schematics/schemeunit:3)
         "unet.ss")


(define-unet define-checks
  (import in-vector
          vector-length)
  (export check-vector=)
          
  (define (check-vector= v1 v2 epsilon)
    (with-check-info
     (['message "Unequal length"])
     (check-eq? (vector-length v1) (vector-length v2)))
    (for ([i  (in-range (vector-length v1))]
          [x1 (in-vector v1)]
          [x2 (in-vector v2)])
         (with-check-info
          (['message (format "Elements ~a and ~a at index ~a are not within ~a"
                             x1 x2 i epsilon)])
          (check <= (abs (- x1 x2)) epsilon))))
  )
         

(provide define-checks)