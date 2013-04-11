#!/usr/bin/guile
!#

(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))

(define (f2 n)
  (define (iter a b c count)
    (if (= count 0)
	    a
		(iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (iter 0 1 2 n))

(define (f3 n)
  (define (iter a b c n)
    (if (< n 3)
	    c
		(iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))
  (if (< n 3)
      n
	  (iter 0 1 2 n)))

(display (f1 5))
(newline)

(display (f2 5))
(newline)

(display (f3 5))
(newline)
