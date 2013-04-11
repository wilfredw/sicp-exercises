#!/usr/bin/guile -s
!#

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) (x))
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
	     ((even? k) 2)
	     (else 4))
       (f (+ a (* k h)))))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(define (another-simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2 h)))
  (* (/ h 3.0 ) (+ (f a)
		   (* 4.0 (sum f (+ a h) add-2h (+ a (* (- n 1) h))))
		   (* 2.0 (sum f (+ a (* 2 h)) add-2h (+ a (* (- n 2) h))))
		   (f (+ a (* n h))))))

(display (integral cube 0 1 0.01))
(newline)
;;(display (integral cube 0 1 0.001))
(newline)
(display (simpson-integral cube 0 1 100))
(newline)
;;(display (simpson-integral cube 0 1 1000))
(newline)
(display (another-simpson-integral cube 0 1 100))
(newline)

