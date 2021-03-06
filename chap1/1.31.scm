#lang scheme
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity a)
  (if (= a 0)
      1
      a))

(define (inc x) (+ x 1))

(define (factorial n)
  (product identity 0 inc n))

(define (pi-product b)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (next x) (+ x 1))
  (product-iter pi-term 1 next b))