#lang scheme/base

(require (planet schematics/sake:1))

(define-task compile
  ()
  (action:compile "all-numeric-tests.ss"))

(define-task test
  (compile)
  (action:test "all-numeric-tests.ss" 'all-numeric-tests))

(define-task all
  (test compile))

(define-task default
  (all))


