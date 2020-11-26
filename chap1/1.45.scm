#lang scheme
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x)
    (/ (+ (f x) x)
       2)))

(define (repeated f n)
  (if (= 1 n) f
      (compose f (repeated f (- n 1)))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (cube-rt x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y 2))
                            average-damp
                            1.0)))

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root n)
  (lambda (x)
    (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                              (repeated average-damp (ceiling (log2 n)))
                              1.0)))

;(define (nth-root x n)