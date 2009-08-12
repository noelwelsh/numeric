;; Hash tables as sparse vectors
;;
;; Types:
;;   sparse : synonym for sparse sparse
;;
;; Naming convention
;;   s prefix indicates operates on sparse vector
;;   / suffix indicates a fold
;;   . suffix indicates operates element-wise

#lang scheme/base

(require (for-syntax scheme/base))

(require scheme/dict)

(define smake make-hasheq)

(define (sparse elts)
  (define s (smake))
  (for
   (([idx val] (in-dict elts)))
   (unless (zero? val)
     (sset! s idx val)))
  s)

(define-syntax (for/sparse stx)
  (syntax-case stx ()
    [(for/sparse (for-clause ...) body ...)
     (syntax
      (let ([s (smake)])
        (for (for-clause ...)
             (let-values (([idx val] body ...))
               (sset! s idx val)))
        s))]))

(define in-sparse in-hash)

(define (sref sparse idx)
  (hash-ref sparse idx 0.0))

(define (sset! sparse idx val)
  (unless (zero? val)
    (hash-set! sparse idx val)))

;; smap : (number -> number) sparse -> sparse
(define (smap fn sparse)
  (for/sparse (([idx val] (in-sparse sparse)))
    (values idx (fn val))))

;; smapi : (natural number -> number) sparse -> sparse
(define (smapi fn sparse)
  (for/sparse (([idx val] (in-sparse sparse)))
    (values idx (fn idx val))))


;;; Derived operations


;; Vector operations

(define (s+/ sparse)
  (for/fold ([sum 0]) (([idx val] (in-sparse sparse)))
    (+ sum val)))


;; Vector-scalar operations

(define (sadd! sparse idx val)
  (sset! sparse idx (+ (sref sparse idx) val)))

(define (s/s sparse scalar)
  (smap (lambda (v) (/ v scalar)) sparse))


;; Vector-vector operations

(define (s/. sparse1 sparse2)
  (for/sparse
   (([i1 v1] (in-sparse sparse1)))
   (values i1 (/ v1 (sref sparse2 i1)))))

(define (s*. sparse1 sparse2)
  (for/sparse
   (([i1 v1] (in-sparse sparse1)))
   (values i1 (* v1 (sref sparse2 i1)))))


            

(provide
 smake
 sparse
 for/sparse
 in-sparse
 
 sref
 sset!

 smap
 smapi

 s+/

 sadd!
 s/s
 
 s/.
 s*.)