#lang scheme/base

(require (for-syntax scheme/base
                     scheme/function)
         (for-template scheme/base)
         scheme/function
         "unet.ss")

(define-unet define-comprehensions
  (import type-name     ;; string e.g. vector
          sequence-name ;; symbol e.g. in-vector
          make-vector
          vector?
          vector-ref
          vector-set!
          vector-length)
  (export for/vector
          for/fold/vector
          in-vector
          in-vector-reverse)

  (define-syntax (for/fold/vector stx)
    (syntax-case stx ()
      [(for/fold/vector
        ([accum-id init-expr] ...)
        ([idx length] for-clause ...)
        body ...)
       (syntax
        (for/fold/vector
         ([accum-id init-expr] ...)
         ([idx length 1] for-clause ...)
         body ...))]

      [(for/fold/vector
        ([accum-id init-expr] ...)
        ([idx length n-vectors])
        body ...)
       (syntax
        (let ([l length])
          (for/fold/vector
           ([accum-id init-expr] ...)
           ([idx l n-vectors] [_ (in-range l)])
           body ...)))]
      
      [(for/fold/vector
        ()
        ([idx length n-vectors] for-clause0 for-clause ...)
        body ...)
       (with-syntax ([(v-id ...) 
                      (datum->syntax
                       stx
                       (for/list ([i (in-range (syntax->datum (syntax n-vectors)))])
                                 (gensym 'for-vector))
                       (syntax n-vectors))]
                     [(temp-id ...)
                      (datum->syntax
                       stx
                       (for/list ([i (in-range (syntax->datum (syntax n-vectors)))])
                                 (gensym 'for-vector))
                       (syntax n-vectors))])
         (syntax
          (let* ([l length]
                 [idx 0]
                 [v-id (make-vector l)] ...)
            (begin
              (for (for-clause0 for-clause ...)
                   (let-values (([temp-id ...] (let () body ...)))
                     (vector-set! v-id idx temp-id) ...
                     (set! idx (add1 idx))))
              (values v-id ...)))))]
        
      [(for/fold/vector
        ([accum-id0 init-expr0] [accum-id init-expr] ...)
        ([idx length n-vectors] for-clause0 for-clause ...)
        body ...)
       (with-syntax ([(v-id ...)
                      (datum->syntax
                       stx
                       (for/list ([i (in-range (syntax->datum (syntax n-vectors)))])
                                 (gensym 'for-vector))
                       (syntax n-vectors))]
                     [(temp-id ...)
                      (datum->syntax
                       stx
                       (for/list ([i (in-range (syntax->datum (syntax n-vectors)))])
                                 (gensym 'for-vector))
                       (syntax n-vectors))])
         (syntax
          (let* ([l length]
                 [idx 0]
                 [v-id (make-vector l)] ...)
            (let-values (([accum-id0 accum-id ...]
                          (for/fold ([accum-id0 init-expr0]
                                     [accum-id  init-expr] ...)
                              (for-clause0 for-clause ...)
                            (let-values (([accum-id0 accum-id ... temp-id ...]
                                          (let () body ...)))
                              (vector-set! v-id idx temp-id) ...
                              (set! idx (add1 idx))
                              (values accum-id0 accum-id ...)))))
              (values accum-id0 accum-id ... v-id ...)))))]))

  (define-syntax (for/vector stx)
    (syntax-case stx ()
      [(for/vector (for-clause ...)
                   body ...)
       (syntax (for/fold/vector () (for-clause ...) body ...))]))

  (define (in-vector-reverse v [offset 0])
  (make-do-sequence
   (lambda ()
     (values
      ;; pos->element
      (curry vector-ref v)
      ;; next-pos
      sub1
      ;; initial position
      (- (vector-length v) (add1 offset))
      ;; continue?
      (curry <= 0)
      ;; continue by value
      (lambda (elt) #t)
      ;; continue by value + index
      (lambda (idx val)
        (<= 0 idx))))))
  
  (define (check-ranges who start stop step)
    (unless (exact-nonnegative-integer? start)
      (raise-type-error who "exact non-negative integer" start))
    (unless (exact-nonnegative-integer? stop)
      (raise-type-error who "exact non-negative integer or #f" stop))
    (unless (and (exact-integer? step) (not (zero? step)))
      (raise-type-error who "exact non-zero integer" step))
    (when (and (< start stop) (< step 0))
      (raise-mismatch-error who
                            (format "start: ~a less than stop: ~a but given negative step: "
                                    start stop)
                            step))
    (when (and (< stop start) (> step 0))
      (raise-mismatch-error who
                            (format "start: ~a more than stop: ~a but given positive step: "
                                    start stop)
                            step)))
  
  (define in-vector-sequence
    (case-lambda
     [(v) (in-vector-sequence v 0 #f 1)]
     [(v start) (in-vector-sequence v start #f 1)]
     [(v start stop) (in-vector-sequence v start stop 1)]
     [(v start stop step)
      (unless (vector? v) (raise-type-error sequence-name type-name v))
      (let ([stop (or stop (vector-length v))])
        (check-ranges sequence-name start stop step)
        (make-do-sequence
         (lambda ()
           (values
            ;; pos->element
            (lambda (i) (vector-ref v i))
            ;; next-pos
            ;; Minor optimisation.  I assume add1 is faster than \x.x+1
            (if (= step 1) add1 (lambda (i) (+ i step)))
            ;; initial pos
            start
            ;; continue?
            (if (> step 0)
                (lambda (i) (< i stop))
                (lambda (i) (> i stop)))
            void
            void))))]))
  
  (define-for-syntax (in-vector-clause stx)
    (syntax-case stx ()
      ;; Fast case
      [((id) (_ vec-expr))
       #'[(id)
          (:do-in
           ;;outer bindings
           ([(vec len) (let ([vec vec-expr])
                         (unless (vector? vec)
                           (in-vector-sequence vec))
                         (values vec (vector-length vec)))])
           ;; outer check
           #f
           ;; loop bindings
           ([pos 0])
           ;; pos check
           (pos . < . len)
           ;; inner bindings
           ([(id) (vector-ref vec pos)])
           ;; pre guard
           #t
           ;; post guard
           #t
           ;; loop args
           ((add1 pos)))]]
      ;; General case
      [((id) (_ vec-expr start))
       (in-vector-clause (syntax ((id) (_ vec-expr start #f 1))))]
      [((id) (_ vec-expr start stop))
       (in-vector-clause (syntax ((id) (_ vec-expr start stop 1))))]
      [((id) (_ vec-expr start stop step))
       #`[(id)
          (:do-in
           ;; Outer bindings
           ;; Prevent multiple evaluation
           ([(v* stop*) (let ([vec vec-expr]
                              [stop* stop])
                          (if (and (not stop*) (vector? vec))
                              (values vec (vector-length vec))
                              (values vec stop*)))]
            [(start*) start]
            [(step*) step])
           ;; Outer check
           (when (or (not (vector? v*))
                     (not (exact-integer? start*))
                     (not (exact-integer? stop*))
                     (not (exact-integer? step*))
                     (zero? step*)
                     (and (< start* stop*) (< step* 0))
                     (and (> start* stop*) (> step* 0)))
             ;; Let in-vector-sequence report the error
             (in-vector-sequence v* start* stop* step*))
           ;; Loop bindings
           ([idx start*])
           ;; Pos guard
           #,(cond
              [(not (number? (syntax-e #'step)))
               #`(if (step* . >= . 0) (< idx stop*) (> idx stop*))]
              [((syntax-e #'step) . >= . 0)
               #'(< idx stop*)]
              [else
               #'(> idx stop*)])
           ;; Inner bindings
           ([(id) (vector-ref v* idx)])
           ;; Pre guard
           #t
           ;; Post guard
           #t
           ;; Loop args
           ((+ idx step)))]]
           [_ #f]))
  
  (define-sequence-syntax in-vector
    (lambda () #'in-vector-sequence)
    in-vector-clause)
    
  )

(provide define-comprehensions)