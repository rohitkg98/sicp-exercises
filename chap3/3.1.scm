#lang scheme
(define (make-accumulator init)
  (lambda (next)
    (begin (set! init (+ init next))
           init)))

(define A (make-accumulator 5))

(A 10)
(A 10)