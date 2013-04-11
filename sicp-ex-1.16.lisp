#!/usr/bin/guile -s
!#

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
	      ((even? n) (iter (square b) (/ n 2) a))
		  (else (iter b (- n 1) (* b a)))))
  (iter b n 1))


(display (fast-exp-iter 2 7))
(newline)
