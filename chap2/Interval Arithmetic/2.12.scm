#lang scheme
(define (make-interval a b) (cons (min a b) (max a b)))

(define upper-bound cdr)

(define lower-bound car)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((width (* c (/ p 100.0))))
    (make-center-width c width)))