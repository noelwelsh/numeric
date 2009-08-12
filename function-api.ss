#lang scheme/base

(require "unet.ss")

(define-unet define-functions
  (import for/vector
          in-vector
          vector-set!
          vector-ref
          vector-length
          vector-find-firsti)
  (export vector* vector+ vector/ vector- 
          vector*s vector/s vector+s vector-s 
          vector-sum vector-product
          vector-dot
          vector-normalise vector-max vector-maxi vector-min vector-mini
          vector-adjoin vector-append
          vector-remove vector-remove-first vector-removei)

  ;; Vector/vector element-wise
  (define (vector* v1 v2)
    (for/vector ([i (vector-length v1)]
                 [x1 (in-vector v1)]
                 [x2 (in-vector v2)])
                (* x1 x2)))

  (define (vector+ v1 v2)
    (for/vector ([i (vector-length v1)]
                 [x1 (in-vector v1)]
                 [x2 (in-vector v2)])
                (+ x1 x2)))
  
  (define (vector/ v1 v2)
    (for/vector ([i (vector-length v1)]
                 [x1 (in-vector v1)]
                 [x2 (in-vector v2)])
                (/ x1 x2)))
  
  (define (vector- v1 v2)
    (for/vector ([i (vector-length v1)]
                 [x1 (in-vector v1)]
                 [x2 (in-vector v2)])
                (- x1 x2)))
  
  
  ;; Vector/scalar functions
  (define (vector*s v s)
    (for/vector ([i (vector-length v)]
                 [x (in-vector v)])
                (* x s)))
  
  (define (vector+s v s)
    (for/vector ([i (vector-length v)]
                 [x (in-vector v)])
                (+ x s)))
  
  (define (vector/s v s)
    (for/vector ([i (vector-length v)]
                 [x (in-vector v)])
                (/ x s)))
  
  (define (vector-s v s)
    (for/vector ([i (vector-length v)]
                 [x (in-vector v)])
                (- x s)))
    
  ;; Reduce functions
  (define (vector-sum v)
    (for/fold ([sum 0]) ([x (in-vector v)])
              (+ sum x)))
  
  (define (vector-product v)
    (for/fold ([prod 1]) ([x (in-vector v)])
              (* prod x)))
  
  
  ;; Vector/vector other
  (define (vector-dot v1 v2)
    (for/fold ([sum 0])
              ([x (in-vector v1)]
               [y (in-vector v2)])
              (+ sum (* x y))))
  
  
  ;; General functions
  (define (vector-normalise v)
    (vector/s v (vector-sum v)))
  
  (define (vector-max v)
    (for/fold ([best -inf.0])
              ([x (in-vector v)])
              (if (> x best) x best)))
  
  (define (vector-maxi v)
    (let-values (([best idx]
                  (for/fold ([best -inf.0] [idx #f])
                            ([x (in-vector v)]
                             [i (in-naturals)])
                            (if (> x best)
                                (values x i)
                                (values best idx)))))
      idx))
  
  (define (vector-min v)
    (for/fold ([best +inf.0])
              ([x (in-vector v)])
              (if (< x best) x best)))
  
  (define (vector-mini v)
    (let-values (([best idx]
                  (for/fold ([best +inf.0] [idx #f])
                            ([x (in-vector v)]
                             [i (in-naturals)])
                            (if (< x best)
                                (values x i)
                                (values best idx)))))
      idx))
  
  (define (vector-adjoin v x)
    (define limit (vector-length v))
    (for/vector ([i (add1 limit)])
                (if (= limit i)
                    x
                    (vector-ref v i))))
  
  (define vector-append
    (case-lambda
      [(v1 v2)
       (let ([v (make-vector (+ (vector-length v1) (vector-length v2)) 0)]
             [offset (vector-length v1)])
         (for ([i (in-range (vector-length v1))]
               [x (in-vector v1)])
           (vector-set! v i x))
         (for ([i (in-range (vector-length v2))]
               [x (in-vector v2)])
         (vector-set! v (+ offset i) x))
         v)]
      [vs
       (let* ([l (for/fold ([l 0]) ([v (in-list vs)])
                   (+ l (vector-length v)))]
              [r (make-vector l 0)]
              [offset 0])
         (for* ([v (in-list vs)]
                [x (in-vector v)])
           (vector-set! r offset x)
           (set! offset (add1 offset)))
         r)]))

  (define (vector-remove v condition)
    (define n (vector-length v))
    (define remove?
      (if (procedure? condition)
          condition
          (lambda (x) (equal? x condition))))
    (define n-left 0)
    (define left
      (let loop ([i 0])
        (if (equal? i n)
            null
            (let ([x (vector-ref v i)])
              (if (remove? x)
                  (loop (add1 i))
                  (begin
                    (set! n-left (add1 n-left))
                    (cons x (loop (add1 i)))))))))

    (for/vector ([i n-left]
                 [x (in-list left)])
                x))

  (define (vector-remove-first v condition)
    (define idx (vector-find-firsti v condition))
    (if idx
        (vector-removei v idx)
        v))

  (define (vector-removei v i)
    (define n (sub1 (vector-length v)))
    (if (<= 0 i n)
        (let ([r (make-vector n)])
          (for ([x (in-vector v 0 i)]
                [j (in-naturals)])
               (vector-set! r j x))
          (for ([x (in-vector v (add1 i))]
                [j (in-range i n)])
               (vector-set! r j x))
          r)
        (raise-mismatch-error
         'vector-removei
         (format "Index ~a not in range [0,~a] for vector ~a." i n v)
         i)))
  )  


(provide define-functions)

