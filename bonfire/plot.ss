#lang scheme/base

(require scheme/class
         scheme/match
         scheme/gui/base
         "point.ss"
         (except-in "frame.ss"
                    Mark Dot Circle Box Line Frame Overlay Panel
                    Colour Style))

;(define padding 20)

;; Hack so match can work with Typed Scheme structs
(define-struct Mark () #:omit-define-values)
(define-struct Dot () #:omit-define-values)
(define-struct Circle (radius) #:omit-define-values)
(define-struct Box (width height) #:omit-define-values)
(define-struct Line (start-x start-y end-x end-y) #:omit-define-values)
(define-struct Frame (offset-x offset-y left top width height style) #:omit-define-values)
(define-struct (Panel Frame) (mark) #:omit-define-values)
(define-struct (Overlay Frame) (parts) #:omit-define-values)
(define-struct Colour (r g b a) #:omit-define-values)
(define-struct Style (outline fill) #:omit-define-values)


;;; Scale

(define current-scale (make-parameter 1))

;;; Styles

(define default-style
  (make-Style (make-Colour 0 0 0 1)
              (make-Colour 255 255 255 1)))

(define current-style (make-parameter default-style))

(define (apply-style dc style)
  ;; We only care about outline for now.
  (send dc set-pen
        (make-object pen% (Colour->color% (Style-outline style)) (/ (current-scale)) 'solid)))

(define-syntax with-style
  (syntax-rules ()
    [(with-style (dc s) expr ...)
     (begin0
         (let ([s* s])
           (if s*
               (parameterize ([current-style s*])
                 (apply-style dc s*)
                 expr ...)
               (begin expr ...)))
       (apply-style dc (current-style)))]))

;; (Colour -> color%)
(define (Colour->color% c)
  ;; MrEd doesn't support alpha in colours, so we ignore it
  ;; for now. (MrEd does support a global alpha.)
  (make-object color% (Colour-r c) (Colour-g c) (Colour-b c)))


;; (: draw-frame (dc<%> point Frame -> void))
(define (draw-frame dc origin f)
  (define x (point-x origin))
  (define y (point-y origin))
  (define s (Frame-style f))

  (with-style (dc s)
    (match f
      [(struct Panel (ox oy l t w h s mk))
       (define new-origin (vector-immutable (+ x ox) (+ y oy)))
       (draw-mark dc new-origin mk)]
      [(struct Overlay (ox oy l t w h s parts))
       (define new-origin (vector-immutable (+ x ox) (+ y oy)))
       (for-each (lambda (f)
                   (draw-frame dc new-origin f))
                 parts)]
      [(struct Frame (ox oy l t w h s))
       (void)])))


;;(: draw-mark (dc<%> origin Mark -> Void))
(define (draw-mark dc origin mk)
  (define x (point-x origin))
  (define y (point-y origin))
  
  (match mk
   [(struct Dot ())
    (send dc draw-rectangle (- x 0.0625) (- y 0.0625) 0.125 0.125)]
   [(struct Circle (r))
    (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r))]
   [(struct Box (w h))
    (define w/2 (/ w 2))
    (define h/2 (/ h 2))
    (send dc draw-rectangle (- x w/2) (- y h/2) w h)]
   [(struct Line (s-x s-y e-x e-y))
    (send dc draw-line (+ x s-x) (+ y s-y) (+ x e-x) (+ y e-y))]))


(provide
 draw-frame
 dot
 circle
 box
 overlay

 overlays
 dots
 circles
 lines

 current-scale)