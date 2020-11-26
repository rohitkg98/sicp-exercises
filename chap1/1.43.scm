#lang scheme
(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= 1 n) f
      (compose f (repeated f (- n 1)))))

; has nice optimization challenges


