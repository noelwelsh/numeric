#lang scheme/base

(require (for-syntax scheme/base))

(define-syntax (for/sum stx)
  (syntax-case stx ()
    [(for/sum (for-clause ...) body0 body ...)
     (syntax
      (for/fold/derived stx
        ([sum 0])
        (for-clause ...)
        (+ sum
           (begin body0 body ...))))]))

(provide for/sum)
       
      