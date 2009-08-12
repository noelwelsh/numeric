#lang scheme/base

(require (for-syntax scheme/base
                     "api.ss"))

(require scheme/unit
         "api-sig.ss")


(define-syntax (define-unit-names stx)
  (syntax-case stx ()
    [(define-unit-names prefix)
     (with-syntax
         ([(external-name ...) (names->ids names stx 'vector)]
          [(internal-name ...) (names->ids names stx (syntax prefix))])
      (syntax
       (begin
         (define external-name internal-name) ...)))]))

(define-syntax (define-api-unit stx)
  (syntax-case stx ()
    [(define-api-unit name prefix)
     (syntax
      (begin
        (define-unit-names prefix)
        (define-unit-from-context name vector^)))]))


(provide define-api-unit)
