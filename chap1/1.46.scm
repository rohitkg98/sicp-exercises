#lang scheme
(define (iterative-improve improve good-enough?)
  (define (method guess x)
    (if (good-enough? guess x)
        guess
        (method (improve guess x) x)))
  method)

(define (generate-sqrt tolerance)
  (iterative-improve (lambda (guess x) (/ (+ guess (/ x guess)) 2))
                     (lambda (guess x) (< (abs (- (* guess guess) x)) tolerance))))

(define (sqrt guess x)
  ((generate-sqrt 0.001) guess x))

(define (generate-fixed-point f tolerance)
  (iterative-improve (lambda (guess x) (f guess))
                     (lambda (guess x) (< (abs (- guess (f guess))) tolerance))))

(define (fixed-point f first-guess)
  ((generate-fixed-point f 0.00001) first-guess 1.0))
