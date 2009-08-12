#lang scheme/base

(require
 scheme/contract
 (planet williams/science:3/science)
 "vector.ss"
 "for.ss")

;; random-dirichlet : vector -> matrix
(define (random-dirichlet params)
  (let ([samples
         (for/vector ([i (vector-length params)]
                      [x (in-vector params)])
           (random-gamma x 1))])
    (vector/s samples (vector-sum samples))))

(provide random-dirichlet)
  
  
(define dirichlet-pdf-contract
  (->d ([point  vector?]
        [params (lambda (d)
                  (= (vector-length point)
                     (vector-length d)))])
       ()
       [_ number?]))

;; dirichlet-pdf : vector vector -> real
;;
;; Elements of point must be in the range (0,1)
(define (dirichlet-pdf point params)
  (let ([point-sum (vector-sum point)])
    (if (= point-sum 1.0)
        (exp (lndirichlet-pdf point params))
        0.0)))

(define (lndirichlet-pdf point params)
  (let ([B (- (lngamma (vector-sum params))
              (vector-sum (vector-map lngamma params)))])
    (+ (for/sum ([p (in-vector point)]
                 [i (in-naturals)])
                (let ([alpha (sub1 (vector-ref params i))])
                  (* (log p) alpha)))
       B)))

  ;; dirichlet without the logs
;   (let* ([params (dirichlet-params dirichlet)]
;           [point-sum (sum-ec (:vector p point) p)]
;           [B (/ (product-ec (:vector p params) (gamma p))
;                 (gamma (sum-ec (:vector p params) p)))])
;      (if (= point-sum 1.0)
;          (/ (product-ec (:vector p (index i) point)
;                         (:let alpha (sub1 (vector-ref params i)))
;                         (expt p alpha))
;             B)
;          0.0))
  

(define (dirichlet-same-dimension-contract return)
  (->d ([samples vector?]
        [params  (lambda (d)
                   (= (vector-length samples)
                      (vector-length d)))])
       ()
       [_ return]))

(define (dirichlet-bayesian-update samples params)
  (vector+ samples params))

(define (dirichlet-bayesian-retract samples params)
  (vector- params samples))

;; dirichlet-sample-likelihood : vector vector -> float
;;
;; We have a data item, and a Dirichlet distribution.  We
;; would like to calculate the likelihood [P(D|H)] of the
;; data given the Dirichlet.  This involves integrating over
;; all multinomials distributed according to the Dirichlet.
;; There is a standard formula for this, e.g. Eqs 49,50 in
;;
;;  http://research.microsoft.com/~minka/papers/dirichlet/minka-dirichlet.pdf
;;
;; gamma(sum(k, alpha(k)) / gamma(sum(k, n(k) + alpha(k))) *
;;   product(k, gamma(n(k) + alpha(k)) / gamma(alpha(k))
(define (dirichlet-sample-likelihood samples params)
  (exp
   (+ (- (lngamma (vector-sum params))
         (lngamma (for/sum
                   ([p (in-vector params)]
                    [s (in-vector samples)])
                   (+ p s))))
      (for/sum
       ([p (in-vector params)]
        [s (in-vector samples)])
       (- (lngamma (+ p s))
          (lngamma p))))))
  
(provide/contract
 [dirichlet-pdf dirichlet-pdf-contract]
 [dirichlet-bayesian-update (dirichlet-same-dimension-contract vector?)]
 [dirichlet-bayesian-retract (dirichlet-same-dimension-contract vector?)]
 [dirichlet-sample-likelihood (dirichlet-same-dimension-contract number?)])
