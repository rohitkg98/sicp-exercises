#lang scheme
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display "Guess Value: ")
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-power-x y guess)
  (fixed-point (lambda (x) (/ (log y) (log x))) guess))

(define (x-power-x-damped y guess)
  (fixed-point (lambda (x) (/ (+ x (/ (log y) (log x))) 2)) guess))

(define (x-power-x-1000 guess)
  (x-power-x 1000 guess))

(define (x-power-x-1000-damped guess)
  (x-power-x-damped 1000 guess))

