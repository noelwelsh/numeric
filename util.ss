#lang scheme/base

(require (for-syntax scheme/base))

(require
 (planet "srfi-4-comprehensions.ss" ("wmfarr" "srfi-4-comprehensions.plt" 1))
 (planet schematics/schemeunit:3)
 "matrix.ss"
 "f64vector.ss")

(provide (all-defined-out))

(define-check (check-m= m1 m2 epsilon)
  (with-check-info
   (['message "Unequal number of rows"])
   (check-eq? (mrows m1) (mrows m2)))
  (with-check-info
   (['message "Unequal number of columns"])
   (check-eq? (mcols m1) (mcols m2)))
  (for* ([r (in-range (mrows m1))]
         [c (in-range (mcols m1))])
   (let ([x1  (mref m1 r c)]
         [x2  (mref m2 r c)])
   (with-check-info
    (['message (format "Elements ~a and ~a at row ~a, col ~a not within ~a"
                       x1 x2 r c epsilon)])
    (check <= (abs (- x1 x2)) epsilon)))))

(define-check (check-v= v1 v2 epsilon)
  (with-check-info
   (['message "Unequal length"])
   (check-eq? (vlength v1) (vlength v2)))
  (for ([i  (in-range (vlength v1))]
        [x1 (in-f64vector v1)]
        [x2 (in-f64vector v2)])
     (with-check-info
      (['message (format "Elements ~a and ~a at index ~a are not within ~a"
                         x1 x2 i epsilon)])
      (check <= (abs (- x1 x2)) epsilon))))

(define in-2d-range
    (case-lambda
      [(b) (in-range 0 b 1)]
      [(a b) (in-range a b 1)]
      [(a b step)
       (unless (real? a) (raise-type-error 'in-range "real-number" a))
       (unless (real? b) (raise-type-error 'in-range "real-number" b))
       (unless (real? step) (raise-type-error 'in-range "real-number" step))
       (make-do-sequence (lambda ()
                                (values
                                 (lambda (x) x)
                                 (lambda (x) (+ x step))
                                 a
                                 (if (step . >= . 0)
                                     (lambda (x) (< x b))
                                     (lambda (x) (> x b)))
                                 (lambda (x) #t)
                                 (lambda (x y) #t))))]))

(define-sequence-syntax *in-2d-range
    (lambda () #'in-range)
    (lambda (stx)
      (let loop ([stx stx])
        (syntax-case stx ()
          [[(id) (_ a b step)] #`[(id)
                                  (:do-in
                                   ;; outer bindings:
                                   ([(start) a] [(end) b] [(inc) step])
                                   ;; outer check:
                                   (unless (and (real? start) (real? end) (real? inc))
                                     ;; let `in-range' report the error:
                                     (in-range start end inc))
                                   ;; loop bindings:
                                   ([pos start])
                                   ;; pos check
                                   #,(cond
                                       [(not (number? (syntax-e #'step)))
                                        #`(if (step . >= . 0) (< pos end) (> pos end))]
                                       [((syntax-e #'step) . >= . 0)
                                        #'(< pos end)]
                                       [else
                                        #'(> pos end)])
                                   ;; inner bindings
                                   ([(id) pos])
                                   ;; pre guard
                                   #t
                                   ;; post guard
                                   #t
                                   ;; loop args
                                   ((+ pos inc)))]]
          [[(id) (_ a b)] (loop #'[(id) (_ a b 1)])]
          [[(id) (_ b)] (loop #'[(id) (_ 0 b 1)])]
          [_ #f]))))

 