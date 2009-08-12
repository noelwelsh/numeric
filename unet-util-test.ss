#lang scheme/base

(require syntax/stx
         (planet schematics/schemeunit:3/test)
         (for-syntax scheme/base
                     syntax/kerncase
                     "unet-util.ss"))

(define-for-syntax exprs
  (list #'(define-syntax foo
            (syntax-rules ()
              [(foo x) (define x 1)]))
        #'(define (bar x)
          (+ x 1))))

(define-syntax (sort stx)
  (syntax-case stx ()
    [(sort)
     (let-values (([transformers expressions] (sort-exprs exprs)))
       #`(list (quote-syntax #,transformers)
               (quote-syntax #,expressions)))]))

(define-syntax (id stx)
  (syntax-case stx ()
    [(id)
     (let* ([ids (transformer-ids
                  (local-expand (car exprs) 'module (kernel-form-identifier-list)))]
            [syms (syntax->datum ids)])
       #`(quote #,syms))]))

(define-syntax (exports stx)
  (syntax-case stx ()
    [(exports)
     (let-values (([transformer-exports value-exports]
                   (sort-exports (list #'foo) (list #'foo #'bar))))
       #`(quote (#,(map syntax->datum transformer-exports)
                #,(map syntax->datum value-exports))))]))

(define-syntax (assoc-list stx)
  (syntax-case stx ()
    [(assoc-list)
     (let ([alist (make-id-assoc-list (list #'foo)
                                      (list #'bar #'foo #'baz)
                                      (list #'real-bar #'real-foo #'real-baz))])
       #`(quote #,(for/list ([kons (in-list alist)])
                            (cons (syntax->datum (car kons))
                                  (syntax->datum (cdr kons))))))]))
;(define/provide-test-suite unet-util-tests
  (test-case
   "sort-exprs"
   (let-values (([transformers expressions]
                 (let ([s (sort)]) (values (car s) (cadr s)))))
     (check-equal? (length (stx->list transformers)) 1)
     (check-equal? (length (stx->list expressions)) 1)))

  (test-case
   "transformer-ids"
   (check-equal? (id) '(foo)))

(test-case
 "make-id-assoc-list"
 (check-equal? (assoc-list) '((foo . real-foo))))

(test-case
 "sort-exports"
 (check-equal? (export) '((foo) (bar))))

;  )