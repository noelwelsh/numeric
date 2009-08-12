#lang scheme/base

(require
 (planet schematics/schemeunit:3)
 "unet.ss")

(define-unet basic-unet
  (import foo bar)
  (export baz quux)
  
  (define baz foo)
  (define quux bar))

(basic-unet
 (import (lambda () 'foo)
         (lambda () 'bar))
 (export basic-baz basic-quux))
 
(basic-unet
 (import (lambda () 'burp)
         (lambda () 'belch))
 (export burp-baz burp-quux))
 
(define-unet macro-import-unet
  (import parameterize-form)
  (export foo)

  (define (foo)
    (let ([op (open-output-string)])
      (parameterize-form
          ([current-output-port op])
        (display "foo"))
      (get-output-string op))))

(macro-import-unet
 (import parameterize)
 (export macro-foo))

(define-unet macro-export-unet
  (import)
  (export)
  (export-syntax define-magic-number)

  (define-syntax define-magic-number
    (syntax-rules ()
      [(define-magic-number name)
       (define name 42)]))
  )

(macro-export-unet
 (import)
 (export)
 (export-syntax define-a))

(define-a a)

(define/provide-test-suite unet-tests
  (test-case
   "basic unet"
   (check-eq? (basic-baz) 'foo)
   (check-eq? (basic-quux) 'bar)
   (check-eq? (burp-baz) 'burp)
   (check-eq? (burp-quux) 'belch))

  (test-case
   "macro-import-unet"
   (check-equal? "foo" (macro-foo)))

  (test-case
   "macro-export-unet"
   (check-equal? a 42)))