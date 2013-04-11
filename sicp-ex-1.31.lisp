#!/usr/bin/guile -s
!#

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (identity x) x)
(define (next x) (+ x 1))
(define (factorial n)
  (product identity 1 next n))

(define (pi-product n)
  (define (pi-term k)
    (/ (* (- k 1) (+ k 1))
       (* k k)))
  (define (inc2 k) (+ k 2))
  (* 4.0
     (product pi-term 3 inc2 n)))

(define (product-iterative term a next b)
  (define (iter x result)
    (if (> x b)
        result
	(iter (next x) (* result (term x)))))
  (iter a 1))

