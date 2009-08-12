#lang scheme/base

(require "unet.ss"
         "for.ss")


(define-unet define-selectors
  (import for/vector for/fold/vector in-vector vector-ref vector-length list->vector)
  (export vector-select vector-last vector-find vector-findi vector-find-first vector-find-firsti vector-slice)

  (define (vector-select v mask)
    (define n-selected
      (for/sum ([x (in-vector mask)])
               (if x 1 0)))
    (define-values (i selected)
      (for/fold/vector ([i 0])
                     ([_ n-selected])
        (let loop ([i i])
          (if (vector-ref mask i)
              (values (add1 i) (vector-ref v i))
              (loop (add1 i))))))
    selected)

  (define (vector-last v)
    (vector-ref v (sub1 (vector-length v))))

  (define (vector-find v condition)
    (define accept?
      (if (procedure? condition)
          condition
          (lambda (x) (equal? x condition))))
    (list->vector
     (reverse
      (for/fold ([found null])
                ([x (in-vector v)])
        (if (accept? x)
            (cons x found)
            found)))))

  (define (vector-findi v condition)
    (define accept?
      (if (procedure? condition)
          condition
          (lambda (x) (equal? x condition))))
    (list->vector
     (reverse
      (for/fold ([found null])
                ([x (in-vector v)]
                 [i (in-naturals)])
        (if (accept? x)
            (cons i found)
            found)))))

  (define (vector-find-first v condition)
    (define accept?
      (if (procedure? condition)
          condition
          (lambda (x) (equal? x condition))))
    (let loop ([idx 0])
      (if (= idx (vector-length v))
          #f
          (let ([x (vector-ref v idx)])
            (if (accept? x)
                x
                (loop (add1 idx)))))))

  (define (vector-find-firsti v condition)
    (define accept?
      (if (procedure? condition)
          condition
          (lambda (x) (equal? x condition))))
    (let loop ([idx 0])
      (if (= idx (vector-length v))
          #f
          (let ([x (vector-ref v idx)])
            (if (accept? x)
                idx
                (loop (add1 idx)))))))

  (define vector-slice
    (case-lambda
      [(v end) (vector-slice v 0 end)]
      [(v start end)
       (for/vector ([i (- end start)]
                    [x (in-vector v start end)])
                   x)]))
  )


(provide define-selectors)