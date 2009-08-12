#lang scheme/base

(require (for-syntax scheme/base
                     "api.ss")
         (planet untyped/unlib:3/syntax))

(require scheme/unit)


(define-syntax (define-api-sig stx)
  (syntax-case stx ()
    [(define-api-sig id)
     (with-syntax
         ([exported-names (names->ids names stx 'vector)])
       (syntax
        (define-signature id exported-names)))]))


(define-api-sig vector^)


(provide vector^)