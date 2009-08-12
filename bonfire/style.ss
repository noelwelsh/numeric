#lang typed-scheme

(define-struct: Colour ([r : Integer]
                        [g : Integer]
                        [b : Integer]
                        [a : Number]))

(define-struct: Style ([outline : Colour]
                       [fill : Colour]))

(provide
 (struct-out Colour)
 (struct-out Style))