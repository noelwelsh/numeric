#lang typed-scheme

(define-struct: Colour ([r : Real]
                        [g : Real]
                        [b : Real]
                        [a : Number]))

(define-struct: Style ([outline : Colour]
                       [fill : Colour]))

(provide
 (struct-out Colour)
 (struct-out Style))