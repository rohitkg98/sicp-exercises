#lang scheme
(define a (list 1 3 (list 5 7) 9))

(cadr (caddr a))

(define b (list (list 7)))

(caar b)

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(cadadr (cadadr (cadadr c)))

