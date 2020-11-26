#lang scheme
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= 1 n) f
      (compose f (repeated f (- n 1)))))

(define (average a b c)
  (/ (+ a b c)
     3))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated smooth n) f))