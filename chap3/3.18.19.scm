#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)

(define (looped? xs)
  (define (helper p1 p2)
    (cond ((or (null? p1)
               (null? p2)
               (null? (mcdr p2))) #f)
          ((eq? p1 p2) #t)
          (else (helper (mcdr p1) (mcdr (mcdr p2))))))
  (cond ((or (null? xs)
             (null? (mcdr xs))) #f)
        (else (helper (mcdr xs) (mcdr (mcdr xs))))))