#lang scheme/base

(require syntax/kerncase
         syntax/stx
         (for-syntax scheme/base)
         (for-template scheme/base)
         (for-meta 2 scheme/base)
         (for-meta 3 scheme/base)
         (for-meta -3 scheme/base)
         (for-meta -2 scheme/base))

;; sort-exprs : (stx-listof stx) -> (values (listof expr) (listof expr))
;;
;; Sort a list of syntax into a list of transformer
;; definitions and a list of other expressions. The returned
;; lists are fully expanded.
(define (sort-exprs exprs)
  (let loop ([exprs exprs]
             [transformers null]
             [expressions null])
    (if (null? exprs)
        (values transformers expressions)
        (let ([stx (stx-car exprs)]
              [rest (stx-cdr exprs)])
          (let ([expansion
                 (local-expand stx
                               (syntax-local-context)
                               (kernel-form-identifier-list))])
            (kernel-syntax-case
             expansion
             #f
             [(define-syntaxes (id ...) body ...)
              (loop rest (cons expansion transformers) expressions)]
             [(define-values (id ...) body ...)
              (loop rest transformers (cons expansion expressions))]
             [(define-values-for-syntax (id ...) body ...)
              (loop rest transformers (cons expansion expressions))]))))))

;; sort-exports : (listof stx) (listof stx) -> (values (listof stx) (listof stx))
(define (sort-exports transformer-ids export-ids)
  (for/fold ([ts null]
             [vs null])
      ([export (in-list export-ids)])
    (if (findf (lambda (transformer-id)
                 (free-identifier=? export transformer-id))
               transformer-ids)
        (values (cons export ts) vs)
        (values ts (cons export vs)))))

;; transformer-ids : stx -> (listof stx)
;;
;; Gather the ids from a define-syntaxes expression. expr
;; must be expanded to kernel syntax.
(define (transformer-ids expr)
  (kernel-syntax-case expr #f
    [(define-syntaxes ids body ...)
     (begin
     ;;(printf "ids are ~a\n" #'ids)
     ;;(printf "as lists ~a\n" (syntax->list #'ids))
     ;;(printf "as datum ~a\n" (syntax->datum #'ids))
     (syntax->list #'ids))]))

;; make-id-assoc-list : (listof stx) (listofx stx) -> (alistof stx stx)
;;
;; exports is a list of syntax, the ids that we are searching for
;;
;; export-ids is a list of syntax, the declared export ids
;;
;; real-export-ids is a list of syntax, the real export ids
;;
;; Return an association list of id and real-id, skipping
;; ids where the export and real-export are the same.
(define (make-id-assoc-list exports export-ids real-export-ids)
  (if (null? export-ids)
      null
      (let ([id (stx-car export-ids)]
            [real-id (stx-car real-export-ids)])
        (if (findf (lambda (export-id)
                     (and (bound-identifier=? export-id id)
                          (not (findf (lambda (real-export-id)
                                        (bound-identifier=? real-export-id id))
                                      real-export-ids))))
                   exports)
            (cons (cons id real-id)
                  (make-id-assoc-list
                   exports
                   (cdr export-ids)
                   (cdr real-export-ids)))
            (make-id-assoc-list
             exports
             (cdr export-ids)
             (cdr real-export-ids))))))

(provide
 sort-exprs
 sort-exports
 transformer-ids
 make-id-assoc-list)