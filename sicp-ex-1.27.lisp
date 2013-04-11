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
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n test-times)
  (cond ((= test-times 0) #t)
        ((fermat-test n) (fast-prime? n (- test-times 1)))
        (else #f)))


(define (test-carmichael-number n)
  (define (try-it n a)
    (= (expmod a n n) a))
  (define (fermat-test-iter n a)
    (cond ((>= a n) #t)
          ((try-it n a) (fermat-test-iter n (+ a 1)))
          (else #f)))
  (display n)
  (display " *** ")
  (display (prime? n))
  (display " *** ")
  (display (fermat-test-iter n 2))
  (newline))

(test-carmichael-number 13)
(test-carmichael-number 14)
(test-carmichael-number 561)
(test-carmichael-number 1105)

