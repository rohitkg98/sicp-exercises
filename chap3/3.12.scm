#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)


(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
; (b)

(set! x (list->mlist x))
(set! y (list->mlist y))

(define w (mappend! x y))
w
(mcdr x)
; (b c d)