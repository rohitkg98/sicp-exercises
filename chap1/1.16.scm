#lang scheme
(define (fast-expt-evl b n)
  (define (iter acc odd-acc count)
    (cond ((= count 1) (* acc odd-acc))
          ((even? count) (iter (* acc acc) odd-acc (/ count 2)))
          (else (iter acc (* odd-acc acc) (- count 1)))))
  (iter b 1 n))
