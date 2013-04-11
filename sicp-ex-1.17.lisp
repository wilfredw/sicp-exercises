#!/usr/bin/guile -s
!#

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
		(else (+ a (* a (- b 1))))))

(define (* a b)
  (define (iter a n x)
    (if (= n 0)
        x
        (iter a (- n 1) (+ x a))))
  (iter a b 0))

(define (* a b)
  (define (iter a n x)
    (cond ((= n 0) x)
	      ((even? n) (iter (double a) (halve n) x))
		  (else (iter a (- n 1) (+ x a)))))
  (iter a b 0))

(display 'Hello)
(newline)
