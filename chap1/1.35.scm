#lang scheme

; use -b + sqrt(sq(b) - 4*a*c) / 2

; OR

; put value of golden ratio in fixed point RHS
; and derive the same value as LHS to prove the first part

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

(define (guess-golden-ratio guess)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) guess))
