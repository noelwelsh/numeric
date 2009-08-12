#lang typed-scheme

(require
 scheme/match
 "point.ss"
 "mark.ss"
 "style.ss")

;; A Frame is a container for a Mark
;;
;; The frame gives the x and y offsets of the origin (0,0)
;; for this frame relative to its parent, and the box
;; surrounding all the Frames contained in this Frame. The
;; coordinates of the box are relative to the origin.
;;
;; Marks in a panel are always centered in the
;; panel. I.e. the x and y offsets give the centre of the
;; circle/point/box/etc.
(define-struct: Frame ([offset-x : Number] [offset-y : Number]
                       [left : Number] [top : Number]
                       [width : Number] [height : Number]
                       [style : (U #f Style)]) #:transparent)
(define-struct: (Panel Frame) ([mark : Mark]) #:transparent)
(define-struct: (Overlay Frame) ([parts : (Listof Frame)]) #:transparent)


;;; Constructors

;; This is an empty Frame, useful for the base case of a recursion
(: frame (-> Frame))
(define (frame)
  (make-Frame 0 0  0 0  0 0  #f))

(: dot (Point -> Panel))
(define (dot pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (make-Panel x y  0 0  0 0  #f
              (make-Dot)))

(: circle (Point -> Panel))
(define (circle pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (define r (point-r pt))
  (define d (* 2 r))
  (make-Panel x y  (- r) (- r)  d d  #f
              (make-Circle r)))

(: box (Point -> Panel))
(define (box pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (define w (point-w pt))
  (define h (point-h pt))
  (make-Panel x y  (- (/ w 2)) (- (/ h 2))  w h  #f
              (make-Box w h)))

(: line (Point Point -> Panel))
(define (line start-pt end-pt)
  (define-values (start-x start-y end-x end-y)
    (values (point-x start-pt) (point-y start-pt) (point-x end-pt) (point-y end-pt)))
  (define-values (left-x left-y right-x right-y)
    (if (<= start-x end-x)
        (values start-x start-y end-x end-y)
        (values end-x end-y start-x start-y)))
  (define top-y (min start-y end-y))
  (define width (abs (- end-x start-x)))
  (define height (abs (- end-y start-y)))
  (define centre-x (+ left-x (/ width 2)))
  (define centre-y (+ top-y (/ height 2)))
  
  (make-Panel centre-x centre-y  (- left-x centre-x) (- top-y centre-y)  width height  #f
              (make-Line (- left-x centre-x) (- left-y centre-y)
                         (- right-x centre-x) (- right-y centre-y))))

(: cross (Point -> Frame))
(define (cross pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (overlay (line (vector (- x .5) (- y .5)) (vector (+ x .5) (+ y .5)))
           (line (vector (- x .5) (+ y .5)) (vector (+ x .5) (- y .5)))))

(: overlay (Frame Frame -> Frame))
(define (overlay f1 f2)
  (define-values (l t w h) (frames-bounding-box f1 f2))
  (make-Overlay 0 0  l t  w h  #f (list f1 f2)))

(: overlays ((Listof Frame) -> Frame))
(define (overlays fs)
  ;; for/fold doesn't work in typed scheme, so we get this tedium
  (define coords
    (foldl (lambda (#{f : Frame} #{coords : (Vectorof Number)})
             (define-values (l1 t1 w1 h1)
               (values (vector-ref coords 0)
                       (vector-ref coords 1)
                       (vector-ref coords 2)
                       (vector-ref coords 3)))
             (define-values (l2 t2 w2 h2) (frame->coords f))
             (define-values (l3 t3 w3 h3) (bounding-box l1 t1 w1 h1 l2 t2 w2 h2))
             (vector l3 t3 w3 h3))
           (let-values (((l1 t1 w1 h1) (frame->coords (car fs))))
             (vector l1 t1 w1 h1))
           (cdr fs)))
  #;(define-values (#{l : Number} #{t : Number} #{w : Number} #{h : Number})
    (for/fold ([#{l : Number} l1] [#{t : Number} t1]
               [#{w : Number} w1] [#{h : Number} h1])
        ([f (in-list (cdr fs))])
      (define-values (l2 t2 w2 h2) (frame->coords f))
      (bounding-box l t w h l2 t2 w2 h2)))
  (make-Overlay 0 0  
                (vector-ref coords 0)
                (vector-ref coords 1)
                (vector-ref coords 2)
                (vector-ref coords 3)
                #f
                fs))

(: dots ((Listof Point) -> Frame))
(define (dots pts)
  (overlays (map dot pts)))

(: circles ((Listof Point) -> Frame))
(define (circles pts)
  (overlays (map circle pts)))

(: lines ((Listof (Pair Point Point)) -> Frame))
(define (lines pts)
  (overlays (map (lambda (#{pair : (Pair Point Point)})
                   (line (car pair) (cdr pair)))
                 pts)))

(: crosses ((Listof Point) -> Frame))
(define (crosses pts)
  (overlays (map cross pts)))

;;; Styles

;; Set outline and fill to the same colour
(: colour (Frame (Vectorof Number) -> Frame))
(define (colour f c)
  (define o-x (Frame-offset-x f))
  (define o-y (Frame-offset-y f))
  (define l (Frame-left f))
  (define t (Frame-top f))
  (define w (Frame-width f))
  (define h (Frame-height f))

  (define len (vector-length c))
  (cond
    [(= len 3) 
     (let ([c (make-Colour
               (round (vector-ref c 0))
               (round (vector-ref c 1))
               (round (vector-ref c 2))
               1)])
       (style f (make-Style c c)))]
    [(= len 4)
     (let ([c (make-Colour
               (round (vector-ref c 0))
               (round (vector-ref c 1))
               (round (vector-ref c 2))
               (vector-ref c 3))])
       (style f (make-Style c c)))]
    [else
     (error "colour: Not a valid colour.")]))

(: style (Frame Style -> Frame))
(define (style f s)
  (define o-x (Frame-offset-x f))
  (define o-y (Frame-offset-y f))
  (define l (Frame-left f))
  (define t (Frame-top f))
  (define w (Frame-width f))
  (define h (Frame-height f))

  (cond
   [(Panel? f)
    (make-Panel o-x o-y l t w h s (Panel-mark f))]
   [(Overlay? f)
    (make-Overlay o-x o-y l t w h s (Overlay-parts f))]
   [(Frame? f)
    (make-Frame o-x o-y l t w h s)]))

;;; Utilities

(: frame->coords (Frame -> (values Number Number Number Number)))
(define (frame->coords f)
  (define o-x (Frame-offset-x f))
  (define o-y (Frame-offset-y f))
  (define l (Frame-left f))
  (define t (Frame-top f))
  (define w (Frame-width f))
  (define h (Frame-height f))
  (values (+ o-x l) (+ o-y t) w h))

(: bounding-box (Number Number Number Number Number Number Number Number -> (values Number Number Number Number)))
(define (bounding-box l1 t1 w1 h1 l2 t2 w2 h2)
  (let* ([r1 (+ l1 w1)]
         [r2 (+ l2 w2)]
         [b1 (+ t1 h1)]
         [b2 (+ t2 h2)]
         
         [l3 (min l1 l2)]
         [t3 (min t1 t2)]
         [w3 (- (max r1 r2) l3)]
         [h3 (- (max b1 b2) t3)])
    (values l3 t3 w3 h3)))

(: frames-bounding-box (Frame Frame -> (values Number Number Number Number)))
(define (frames-bounding-box f1 f2)
  (define-values (l1 t1 w1 h1) (frame->coords f1))
  (define-values (l2 t2 w2 h2) (frame->coords f2))
  (bounding-box l1 t1 w1 h1 l2 t2 w2 h2))
  
(provide
 (struct-out Frame)
 (struct-out Panel)
 (struct-out Overlay)

 (all-from-out "mark.ss")
 (all-from-out "style.ss")
 
 frame
 dot
 circle
 box
 line
 cross
 overlay

 colour
 
 overlays
 dots
 circles
 lines
 crosses)