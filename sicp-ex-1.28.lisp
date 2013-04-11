#!/usr/bin/guile -s
!#

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? x)
  (= (remainder x 2) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x xs)
      (if (and (not (= x 1))
               (not (= x (- m 1)))
               (= xs 1))
          0
          xs))
    (check-nontrivial-sqrt1 x (remainder (square x) m))) 
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-with-check (expmod base (/ exp 2) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n test-times)
  (cond ((= test-times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- test-times 1)))
        (else #f)))

(display (fast-prime? 13 10))
(newline)
(display (fast-prime? 14 10))
(newline)
(display (fast-prime? 561 10))
(newline)
(display (fast-prime? 1105 10))
(newline)
