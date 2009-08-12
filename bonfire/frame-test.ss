#lang scheme/base

(require (planet schematics/schemeunit:3/test))
(require "mark.ss"
         "frame.ss")

(define/provide-test-suite frame-tests
  (test-case
   "dot"
   (check-equal? (dot (vector -2 2))
                 (make-Panel -2 2 0 0 0 0 #f (make-Dot))))

  (test-case
   "circle"
   (check-equal? (circle (vector -2 2 6))
                 (make-Panel -2 2 -6 -6 12 12 #f (make-Circle 6))))

  (test-case
   "box"
   (check-equal? (box (vector -2 2 4 8))
                 (make-Panel -2 2 -2 -4 4 8 #f (make-Box 4 8))))

  (test-case
   "line"
   (check-equal? (line (vector -2 -2) (vector 4 -2))
                 (make-Panel 1 -2 -3 0 6 0 #f
                             (make-Line -3 0 3 0)))
   (check-equal? (line (vector -2 -2) (vector 4 4))
                 (make-Panel 1 1 -3 -3 6 6 #f
                             (make-Line -3 -3 3 3)))
   (check-equal? (line (vector -2 -2) (vector -2 4))
                 (make-Panel -2 1  0 -3  0 6  #f
                             (make-Line 0 -3 0 3)))

   (check-equal? (line (vector 4 4) (vector 4 -2))
                 (make-Panel 4 1  0 -3 0 6  #f
                             (make-Line 0 3 0 -3)))
   (check-equal? (line (vector 4 -2) (vector -2 4))
                 (make-Panel 1 1  -3 -3  6 6  #f
                             (make-Line -3 3 3 -3)))
   (check-equal? (line (vector 4 4) (vector -2 4))
                 (make-Panel 1 4  -3 0 6 0  #f
                             (make-Line -3 0 3 0))))

  (test-case
   "cross"
   (define p (cross (vector 0 0)))
   (check-equal? (Frame-left p) -.5)
   (check-equal? (Frame-top p) -.5)
   (check-equal? (Frame-width p) 1.)
   (check-equal? (Frame-height p) 1.))

  (test-case
   "overlay atomic marks"
   (define f (overlay (circle (vector 0 0 10)) (dot (vector -11 -11))))
   (check-equal? f
                 (make-Overlay 0 0 -11 -11 21 21 #f
                               (list (make-Panel 0 0 -10 -10 20 20 #f
                                                 (make-Circle 10))
                                     (make-Panel -11 -11  0 0  0 0 #f
                                                 (make-Dot))))))

  (test-case
   "overlay overlays"
   (define f (overlay (overlay (circle (vector 0 0 10)) (circle (vector -2 -2 5)))
                      (overlay (circle (vector -10 -10 2)) (circle (vector 3 3 3)))))
   (check-equal? f
                 (make-Overlay 0 0 -12 -12 22 22 #f
                               (list
                                (make-Overlay 0 0 -10 -10 20 20 #f
                                              (list
                                               (make-Panel 0 0 -10 -10 20 20 #f
                                                           (make-Circle 10))
                                               (make-Panel -2 -2 -5 -5 10 10 #f
                                                           (make-Circle 5))))
                                (make-Overlay 0 0 -12 -12 18 18 #f
                                              (list
                                               (make-Panel -10 -10 -2 -2 4 4 #f
                                                           (make-Circle 2))
                                               (make-Panel 3 3 -3 -3 6 6 #f
                                                           (make-Circle 3))))))))

  (test-case
   "overlays"
   (define f (overlays (list (overlay (circle (vector 0 0 5)) (dot (vector 0 0)))
                             (overlay (circle (vector 2 2 5)) (dot (vector 2 2)))
                             (overlay (circle (vector 3 3 2)) (dot (vector 3 3))))))
   (check-equal? f
                 (make-Overlay 0 0 -5 -5 12 12 #f
                               (list
                                (make-Overlay 0 0 -5 -5 10 10 #f
                                              (list
                                               (make-Panel 0 0 -5 -5 10 10 #f
                                                           (make-Circle 5))
                                               (make-Panel 0 0 0 0 0 0 #f
                                                           (make-Dot))))
                                (make-Overlay 0 0 -3 -3 10 10 #f
                                              (list
                                               (make-Panel 2 2 -5 -5 10 10 #f
                                                           (make-Circle 5))
                                               (make-Panel 2 2 0 0 0 0 #f
                                                           (make-Dot))))
                                (make-Overlay 0 0 1 1 4 4 #f
                                              (list
                                               (make-Panel 3 3 -2 -2 4 4 #f
                                                           (make-Circle 2))
                                               (make-Panel 3 3 0 0 0 0 #f
                                                           (make-Dot))))))))

  )