#lang scheme/base

(require scheme/math
         (planet williams/science:3/random-distributions/gaussian)
         "vector.ss"
         "matrix.ss")

(define 2pi (* 2 pi))

;; (vectorof number) (triangular-matrixof number) -> (vectorof number)
;;
;; mu is the mean
;;
;; sigma is the square root of the covariance matrix -- use
;; the cholesky decomposition to calculate this
;;
;; Uses the method given at Wikipedia 
(define (random-multivariate-gaussian mu sigma)
  (define n (vector-length mu))
  (define z
    (for/vector ([i n])
                (random-gaussian 1 0)))
  (vector+ mu (vector*m z sigma)))

;; (vectorof number) (vectorof number) (matrixof number) -> [0,1]
;;
;; x is the point
;;
;; mu is the mean
;;
;; sigma is the cholesky decomposition of the covariance
;; matrix, as created by matrix-cholesky (lower triangle is
;; L, upper is L^T)
(define (multivariate-gaussian-pdf x mu sigma)
  (define n (vector-length mu))
  (define det (matrix-cholesky-determinant sigma))
  (define inverse (matrix-cholesky-invert sigma))
  ;; Normalisation constant
  (define z (/ 1 (* (expt 2pi (/ n 2)) (sqrt det))))
  (define diff (vector- x mu))
  (define p (exp (* -1/2 (vector-dot (vector*m diff inverse) diff))))

  (* z p))

;; (vectorof number) (matrixof number) ->
;;   (values (-> (vectorof number)) ((vectorof number) -> number))
;;
;; mu is the mean
;;
;; sigma is the covariance matrix
;;
;; Returns two functions, the first generates samples, and
;; the second is the PDF. This is more efficient than
;; repeatedly performing the required matrix operations.
(define (make-multivariate-gaussian mu sigma)
  (define n (vector-length mu))
  ;; Cholesky is faster but LU is more numerically stable
  ;; AND valid for singular matrices
  (define cholesky (matrix-cholesky sigma))
  ;;(define det (matrix-cholesky-determinant cholesky))
  ;;(define inverse (matrix-cholesky-invert cholesky))
  (define-values (lu p s) (matrix-lu sigma))
  (define det (matrix-lu-determinant lu s))
  (define inverse (matrix-lu-invert lu p))
  ;; Normalisation constant
  (define z (/ 1 (* (expt 2pi (/ n 2)) (sqrt det))))

  (values
   ;; Sampler
   (lambda ()
     (random-multivariate-gaussian mu cholesky))
   ;; PDF
   (lambda (x)
       (define diff (vector- x mu))
       (* z (exp (* -1/2 (vector-dot (vector*m diff inverse) diff)))))))


(provide
 random-multivariate-gaussian
 multivariate-gaussian-pdf
 make-multivariate-gaussian)