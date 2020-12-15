#lang scheme
(define (averager a b c)
  (let ((s (make-connector))
        (k (make-connector)))
    (adder a b s)
    (multiplier k c s)
    (constant 2 k)))