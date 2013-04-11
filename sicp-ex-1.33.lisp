#!/usr/bin/guile -s
!#

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))


(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (inc n)
  (+ n 1))

(define (sq a) (* a a))

(define (sum-of-prime-squares a b) (filtered-accumulate + 0 sq a inc b prime?))

(define (gcd m n)
  (cond ((< m n) (gcd n m))
        ((= n 0) m)
	(else (gcd n (remainder m n)))))

(define (relative-prime? m n)
  (= (gcd m n) 1))

(define (identity x) x)

(define (product-of-relative-primes n)
  (define (filter x)
    (relative-prime? x n))
  (filtered-accumulate * 1 identity 1 inc n filter))

(display (product-of-relative-primes 10))
(newline)
