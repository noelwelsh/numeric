#lang scheme/base

(require
 (for-syntax scheme/base
             syntax/stx
             "unet-util.ss")
 (for-template scheme/base)
 (for-meta 2 scheme/base)
 (for-meta -2 scheme/base))

(define-for-syntax (make-import-forms import-ids real-import-ids)
  (for/fold ([define-forms null])
      ([import (in-list import-ids)]
       [real-import (in-list real-import-ids)])
    (cond
     [(identifier? real-import)
      (if (free-identifier=? import real-import)
          define-forms
          (cons #`(define-syntax #,import (make-rename-transformer #'#,real-import))
                define-forms))]
     [else
      (cons #`(define #,import #'#,real-import)
            define-forms)])))


(define-syntax (define-unet stx)
  (syntax-case stx (import export)
    [(define-unet name
       (import . import-ids)
       (export . export-ids)
       impl-expr ...)
     ;;(printf "DEFINING UNET ~a\n" (syntax->datum #'name))
     (quasisyntax
      (define-syntax (name stx)
        (syntax-case stx (import export)
          [(_ (import . real-import-ids)
              (export . real-export-ids))
           ;;(printf "STARTING ~a\n" (syntax->datum #'name))
           (let*-values (([transformers expressions]
                          (sort-exprs #'((... (... impl-expr)) ...)))
                         ([transformer-ids]
                          (apply append (map transformer-ids transformers)))
                         ([transformer-exports value-exports]
                          (sort-exports transformer-ids
                                        (syntax->list #'export-ids))))
;;             (printf "transformers: ~a\n" (map syntax->datum transformers))
;;             (printf "expressions: ~a\n" (map syntax->datum expressions))
;;             (printf "transformer-ids: ~a\n" transformer-ids)
;;             (printf "transformer-exports: ~a\n" transformer-exports)
;;             (printf "value-exports: ~a\n" value-exports)
;;             (display "pre-expansion\n")
             (with-syntax ([(import (... ...))
                            (make-import-forms
                             (syntax->list (syntax import-ids))
                             (syntax->list (syntax real-import-ids)))]
                           [((transformer-id . real-transformer-id) (... ...))
                            (make-id-assoc-list transformer-exports
                                                (syntax->list #'export-ids)
                                                (syntax->list #'real-export-ids))]
                           [((value-id . real-value-id) (... ...))
                            (make-id-assoc-list value-exports
                                                (syntax->list #'export-ids)
                                                (syntax->list #'real-export-ids))])
               (let ([expansion
                      #`(begin
                          import (... ...)
                          #,@#'((... (... impl-expr)) ...)
                          (define real-value-id value-id) (... ...)
                          (define-syntax real-transformer-id
                            (make-rename-transformer #'transformer-id)) (... ...))])
                   expansion)
               ))])))]))

(provide define-unet)