#lang scheme/base
;;; extensible-vector.scm  - 26th apr 2009

;;; HISTORY

; 26 mar 2004  -  PLT version
;  2 oct 2004  -  Portable version
;  6 jan 2005  -  PLT PLaneT version
; 26 may 2007  -  Added evector-map, evector-for-each, 
;                 evector-copy and evector-copy from
;                 code by Paulo Matos.
; 26 apr 2009  -  Updated to PLT 4 conventions by Noel Welsh


;;; EXTENSIBLE VECTOR

; This module provides extensible vectors called evectors.
; Setting LENGTH will increase the vectors length,
; new entries will be filled with FILL. Internally the
; vectors of size 16, 32, 64, ... is used in order to
; ensure amortized time O(1). Note that this implies that
; the space used by an evector is not changed by lowering
; the length. 

;;; (make-evector k)
;;; (make-evector k fill)
;;; (make-evector k fill automatic-expansion?)

; Make an evector of length k with the filler fill. 
; The argument automatic-expansion? affects the behaviour
; of evector-set!. If automatic-expansion? is true, then
; setting the field of an index larger than then length
; of the evector will automatically increase the length.
; If automatic-expansion? is true, then evector-set! will
; generate in an error in the same situation.


(require scheme/contract)

;; Contracts

(define (evector-of-length/c l)
  (flat-named-contract
   (format "<evector of length ~a>" l)
   (lambda (obj)
     (and (evector? obj)
          (= (evector-length obj) l)))))

(define (evector-of-length-at-least/c l)
  (flat-named-contract
   (format "<evector of length at least ~a>" l)
   (lambda (obj)
     (and (evector? obj)
          (<= l (evector-length obj))))))
  
  
(define MIN-LENGTH 16)
(define DEFAULT-FILL '())
(define DEFAULT-EXPAND #t)

;; THE %EVECTOR STRUCTURE
      
(define-struct %evector (length vector fill automatic-expansion-on-set!?) #:transparent #:mutable)
      
;; 

(define evector? %evector?)

(define (make-evector k [fill DEFAULT-FILL] [automatic DEFAULT-EXPAND])
  (let ([len (max k MIN-LENGTH)])
    (make-%evector k (make-vector len fill) fill 
                   (or (eq? automatic 'automatic-expansion-on-set!)
                       (eq? automatic #t)))))
      
(define evector-length %evector-length)

(define (evector-ref v i)
  (vector-ref (%evector-vector v) i))
      
(define (evector-set! v i val)
  (cond
   [(< i (%evector-length v))
    (vector-set! (%evector-vector v) i val)]
   [(%evector-automatic-expansion-on-set!? v)
    (begin
      (set-evector-length! v (+ i 1))
      (evector-set! v i val))]
   [else
    (raise-mismatch-error
     'evector-set!
     (format "index out of range [0,~a) and evector does not automatically expand " (%evector-length v))
     i)]))
      
;; this version fills after size
(define (set-evector-length! v l)
  (let ([max-len (vector-length (%evector-vector v))])
    (cond
     [(<= 0 l max-len) (set-%evector-length! v l)]
     [(> l max-len)    (begin
                         (expand-evector! v l)
                         (let ([old-len (%evector-length v)])
                           (set-evector-length! v l)
                           (evector-sub-fill! v old-len l)))])))
      
(define (evector-sub-fill! v start end [fill (%evector-fill v)])
  (let ([w    (%evector-vector v)]
        [fill (%evector-fill v)])
    (do ([i start (add1 i)])
        [(= i end) (void)]
      (vector-set! w i fill))))
      
(define (expand-evector! v l)
  (cond
   [(<= (* 2 l) (%evector-length v))  
    (void)]
   [else                            
    (let* ([new-size   (do ([len (* 2 (vector-length (%evector-vector v))) (* 2 len)])
                           [(<= (* 2 l) len) len])]
           [new-vector (make-vector new-size (%evector-fill v))]
           [old-vector (%evector-vector v)]
           [old-size   (vector-length old-vector)]
           [length     (%evector-length v)])
      (do ([i 0 (add1 i)])
          [(= i length) (void)]
        (vector-set! new-vector i (vector-ref old-vector i)))
      (set-%evector-vector! v new-vector))]))
      
      
;; CONVENIENCE FUNCTIONS SIMILAR TO THE R5RS VECTOR OPERATIONS
      
(define (evector . os)
  (let ([ev (make-evector (length os) #f #t)])
    (do ([os os (cdr os)]
         [i  0  (+ i 1)])
        [(null? os) ev]
      (evector-set! ev i (car os)))))
      
(define (evector->list ev)
  (let ([len (evector-length ev)])
    (do ([i (- len 1) (- i 1)]
         [l  '()      (cons (evector-ref ev i) l)])
        [(< i 0) l])))
      
(define (list->evector l)
  (let ([ev (make-evector (length l) '() #t)])
    (do ([i 0 (+ i 1)]
         [l l (cdr l)])
        [(null? l) ev]
      (evector-set! ev i (car l)))))
      
(define (evector->vector ev)
  (let* ([len   (evector-length ev)]
         [v     (make-vector len)])
    (do ([i 0 (+ i 1)])
        [(= i len) v]
      (vector-set! v i (evector-ref ev i)))))
      
(define (vector->evector v)
  (let* ([len (vector-length v)]
         [ev (make-evector len '() #t)])
    (do ([i 0 (+ i 1)])
        [(= i len) ev]
      (evector-set! ev i (vector-ref v i)))))
      
(define evector-fill!
  (case-lambda
    [(ev val)
     (evector-fill! ev val 0 (evector-length ev))]
    [(ev val start)
     (evector-fill! ev val start (evector-length ev))]
    [(ev val start end)
     (let ([max-len (vector-length (%evector-vector ev))])
       (cond
        [(<= 0 end max-len)  (begin
                               (let ([v (%evector-vector ev)])
                                 (do ([i start (+ i 1)])
                                     [(= i end) (void)]
                                   (vector-set! v i val))))]
        [(> end max-len)     (begin
                               (expand-evector! ev end)
                               (set-%evector-length! ev end)
                               (evector-fill! ev val start end))]))]))
      
(define (evector-size ev)
  (vector-length (%evector-vector ev)))

(define (evector-push! ev v)
  (let ([l (%evector-length ev)])
    (evector-set! ev l v)
    l))

(define (evector-pop! ev)
  (let* ([l (%evector-length ev)])
    (set-%evector-length! ev (- l 1))
    (vector-ref (%evector-vector ev) (- l 1))))
      
(define (evector=? ev1 ev2 [= eqv?])
  (and (= (%evector-length ev1) (%evector-length ev2))
       (let ([len (%evector-length ev1)]
             [v1  (%evector-vector ev1)]
             [v2  (%evector-vector ev2)])
         (let loop ([i 0])
           (cond
            [(>= i len) #t]
            [(not (= (vector-ref v1 i) (vector-ref v2 i))) #f]
            [else (loop (+ i 1))])))))
      
(define evector-map
  (case-lambda 
    [(f ev)
     (let* ([len    (%evector-length ev)]
            [v      (%evector-vector ev)]
            [new-ev (make-evector len)]
            [new-v  (%evector-vector new-ev)])
       (do ([i 0 (+ i 1)])
           [(= i len) new-ev]
         (vector-set! new-v i (f (vector-ref v i)))))]
    [(f ev1 ev2)
     (let* ([len    (min (%evector-length ev1)
                         (%evector-length ev2))]
            [v1     (%evector-vector ev1)]
            [v2     (%evector-vector ev2)]
            [new-ev (make-evector len)]
            [new-v  (%evector-vector new-ev)])
       (do ([i 0 (+ i 1)])
           [(= i len) new-ev]
         (vector-set! new-v i (f (vector-ref v1 i) (vector-ref v2 i)))))]
    [(f . evs)
     (let* ([len    (apply min (map %evector-length evs))]
            [vs     (map %evector-vector evs)]
            [new-ev (make-evector len)]
            [new-v  (%evector-vector new-ev)])
       (do ([i 0 (+ i 1)])
           [(= i len) new-ev]
         (vector-set! new-v i (apply f (map (lambda (v) (vector-ref v i)) vs)))))]))
      
(define (evector-copy ev)
  (let* ([v      (%evector-vector ev)]
         [l      (vector-length v)]
         [new-v  (make-vector l)])
    (do ([i 0 (+ i 1)])
        [(= i l) 'done]
      (vector-set! new-v i (vector-ref v i)))
    (make-%evector (%evector-length ev)
                   new-v
                   (%evector-fill ev)
                   (%evector-automatic-expansion-on-set!? ev))))
      
(define evector-for-each
  (case-lambda 
    [(f ev)
     (let ([len    (%evector-length ev)]
           [v      (%evector-vector ev)])
       (do ([i 0 (+ i 1)])
           [(= i len) (void)]
         (f (vector-ref v i))))]
    [(f ev1 ev2)
     (let ([len    (min (%evector-length ev1)
                        (%evector-length ev2))]
           [v1     (%evector-vector ev1)]
           [v2     (%evector-vector ev2)])
       (do ([i 0 (+ i 1)])
           [(= i len) (void)]
         (f (vector-ref v1 i) (vector-ref v2 i))))]
    [(f . evs)
     (let ([len    (apply min (map %evector-length evs))]
           [vs     (map %evector-vector evs)])
       (do ([i 0 (+ i 1)])
           [(= i len) (void)]
         (apply f (map (lambda (v) (vector-ref v i)) vs))))]))
      
  
(provide 
   ; basic
   make-evector
   (rename-out [%evector? evector?]
               [%evector-length evector-length]
               [%evector-fill evector-fill]
               [set-%evector-fill! set-evector-fill!])
   set-evector-length!
   evector-sub-fill!
   evector-fill!
   ; convenience
   evector)

(provide/contract
 [evector-ref (->d ([v (evector-of-length-at-least/c i)] [i natural-number/c])
                   ()
                   any)]
 [evector-set! (->d ([v evector?]
                     [i natural-number/c]
                     [x any/c])
                    ()
                    [out void?])]
 [evector-size (-> evector? any)]

 [evector->list (-> evector? list?)]
 [list->evector (-> list? evector?)]
 [evector->vector (-> evector? vector?)]
 [vector->evector (-> vector? evector?)]
 [evector-push! (-> evector? any/c any)]
 [evector-pop!  (-> (evector-of-length-at-least/c 1) any)]
 [evector=? (->* (evector? evector?)
                 ((-> any/c any/c (one-of/c #t #f)))
                 (one-of/c #t #f))]
 
 [evector-map (case-> (-> procedure? evector? evector?)
                      (-> procedure? evector? evector? evector?)
                      (-> procedure? #:rest (listof evector?) evector?))]
 [evector-copy (-> evector? evector?)]
 [evector-for-each (case-> (-> procedure? evector? any)
                           (-> procedure? evector? evector? any)
                           (-> procedure? #:rest (listof evector?) any))])