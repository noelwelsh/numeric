#lang typed/scheme

(define-struct: Colour ([r : Real]
                        [g : Real]
                        [b : Real]
                        [a : Real]))

(define-struct: Style ([outline : Colour]
                       [fill : Colour]
                       [width : Real]))

(provide
 (struct-out Colour)
 (struct-out Style))