#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list->mlist
                       (list 'a 'b 'c))))