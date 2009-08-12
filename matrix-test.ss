(module matrix-test mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 3)))
  (require (lib "for.ss")
           (lib "cut.ss" "srfi" "26")
           "matrix.ss"
           "f64vector.ss"
           "util.ss")
  
  (provide matrix-tests)

  ;; Due to column order, this is the matrix:
  ;;
  ;;        1 3
  ;;        2 4
  (define simple-matrix (m 2 2 1 2 3 4))
  
  (define matrix-tests
    (test-suite
     "All tests for matrix"

     (test-case
      "matrix*v"
      (define m (matrix 3 3  1 2 3 4 5 6 7 8 9))
      (check vector= (matrix*v m (vector 1 2 1)) (vector 8 20 32)))
     
     (test-case
      "m= with epsilon"
      (check (cut m= <> <> 0.1)
             (m 2 2 1    2    3    4)
             (m 2 2 1.09 1.91 2.99 4.05)))
     
     (test-case
      "v->m creates correct matrices"
      (check m=
             (v->m 2 3 (v 1 2 3))
             (m 6 3
                1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3))
      ;; Test case from VBPOMDP
      (check m= (v->m 1 3 (v 1.7 2.6 3.1))
             (m 3 3
                1.7 2.6 3.1 1.7 2.6 3.1 1.7 2.6 3.1))
      (check m=
             (v->m 3 2 (v 1 2 3) #f)
             (m 3 6
                1 1 1 2 2 2 3 3 3 1 1 1 2 2 2 3 3 3)))

     (test-case
      "mmap"
      (check m= (mmap - (m 2 2 1 2 3 4)) (m 2 2 -1 -2 -3 -4)))

     (test-case
      "m+/-"
      (check v=
             (m+/- simple-matrix) 
             (v 4 6)))

     (test-case
      "m+/:"
      (check v=
             (m+/: simple-matrix)
             (v 3 7)))

     (test-case
      "mcolumn"
      (check v= (mcolumn simple-matrix 0) (v 1 2))
      (check v= (mcolumn simple-matrix 1) (v 3 4)))

     (test-case
      "mzeros"
      (check m= (mzeros 2 2) (m 2 2 0 0 0 0)))

     (test-case
      "mset-column!"
      (let ([matrix (m 2 2 1 2 3 4)])
        (mset-column! matrix 0 (v 5 6))
        (check = (mref matrix 0 0) 5)
        (check = (mref matrix 1 0) 6)))

     (test-case
      "mset-row!"
      (let ([matrix (m 2 2 1 2 3 4)])
        (mset-row! matrix 0 (v 5 6))
        (check = (mref matrix 0 0) 5)
        (check = (mref matrix 0 1) 6)))

     (test-case
      "m*-v"
      (check m=
             (m*-v simple-matrix (v 2 4))
             (m 2 2 2 4 12 16)))

     (test-case
      "m*:v"
      (check m=
             (m*:v simple-matrix (v 2 4))
             (m 2 2 2 8 6 16)))

     (test-case
      "m/:v"
      (check m=
             (m/:v simple-matrix (v 2 4))
             (m 2 2 .5 .5 1.5 1)))

     (test-case
      "m/-v"
      (check-m=
             (m/-v simple-matrix (v 2 4))
             (m 2 2 1/2 1 3/4 1)
             0.01))
     
     (test-case
      "m+/"
      (check = (m+/ simple-matrix) 10))

     (test-case
      "m/s"
      (check m= (m/s simple-matrix 2) (m 2 2 0.5 1 1.5 2)))

     (test-case
      "m+s"
      (check m= (m+s simple-matrix 2) (m 2 2 3 4 5 6)))

     (test-case
      "for/fold/mcols"
      (check m=
             (for/fold/mcols (5 4) () ()
                             (v 0 1 2 3 4))
             (m 5 4  0 1 2 3 4
                     0 1 2 3 4
                     0 1 2 3 4
                     0 1 2 3 4)
             0.00001)
      (check m=
             (for/fold/mcols (5 4) ([a 0]) ([c (in-range 4)])
                             (values
                              (v (+ c a) (+ c a 1) (+ c a 2) (+ c a 3) (+ c a 4))
                              (add1 a)))
             (m 5 4 0 1 2 3 4
                    2 3 4 5 6
                    4 5 6 7 8
                    6 7 8 9 10)
             0.00001))

     (test-case
      "mfilter"
      (check-m= (mfilter abs (m 3 2  -1 -2 -3 -4 -5 -6))
                (m 3 2  1 2 3 4 5 6)
                0.00001))

     (test-case
      "m*."
      (check-m= (m*. (m 2 3  1 2 3 1 2 3)
                     (m 2 3  2 3 4 4 3 2))
                (m 2 3  2 6 12 4 6 6)
                0.0001))

     (test-case
      "for/fold/mcolsi"
      (check m=
             (for/fold/mcolsi
              (5 4) (c (in-range 3 -1 -1)) () ()
              (v 0 1 2 3 4))
             (m 5 4  0 1 2 3 4
                     0 1 2 3 4
                     0 1 2 3 4
                     0 1 2 3 4)
             0.00001)
      (check m=
             (for/fold/mcolsi
              (5 4) (x (in-range 3 -1 -1)) ([a 0]) ([c (in-range 4)])
              (values
               (v (+ c a) (+ c a 1) (+ c a 2) (+ c a 3) (+ c a 4))
               (add1 a)))
             (m 5 4  6 7 8 9 10
                     4 5 6 7 8
                     2 3 4 5 6                     
                     0 1 2 3 4)
             0.00001))

     (test-case
      "for/fold/mrows"
      (check m=
             (for/fold/mrows (5 4) () ()
                             (v 0 1 2 3))
             (m 5 4  0 0 0 0 0
                     1 1 1 1 1
                     2 2 2 2 2
                     3 3 3 3 3)
             0.00001)
      (check m=
             (for/fold/mrows (5 4) ([a 0]) ([r (in-range 5)])
                             (values
                              (v (+ r a) (+ r a 1) (+ r a 2) (+ r a 3))
                              (add1 a)))
             (m 5 4 0 2 4 6 8
                    1 3 5 7 9
                    2 4 6 8 10
                    3 5 7 9 11)
             0.00001))
     ))
  )
