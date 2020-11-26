#lang scheme

(define (reverse items)
  (define (iter items res)
    (if (null? items) res
        (iter (cdr items) (cons (car items) res))))
  (iter items '()))

(define (deep-reverse x)
  (if (list? x)
      (reverse (map deep-reverse x))
      x)) 

(define x (list (list 1 2) (list 3 4)))

(reverse x)
; ((3 4) (1 2))

(deep-reverse x)
; ((4 3) (2 1))