#lang typed/scheme

;; A Mark is a graphical element of a drawing. It has no
;; location -- that is determined by the frame the mark is
;; located in.
;;
;; A Mark is always centered in its frame, and the centre
;; always has coordinates 0,0.
;;
;; A user normally never deals with Marks directly, instead
;; dealing with Frames.
;;
(define-struct: Mark () #:transparent)
(define-struct: (Dot Mark) () #:transparent)
(define-struct: (Box Mark) ([width : Real] [height : Real]) #:transparent)
(define-struct: (Circle Mark) ([radius : Real]) #:transparent)
(define-struct: (Line Mark) ([start-x : Real] [start-y : Real]
                             [end-x : Real]   [end-y : Real]) #:transparent)

(provide
 (struct-out Mark)
 (struct-out Dot)
 (struct-out Box)
 (struct-out Circle)
 (struct-out Line))

