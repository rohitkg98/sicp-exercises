#lang scheme
; 2.17
(define (last-pair in)
  (if (null? (cdr in)) in
      (last-pair (cdr in))))

; 2.18
(define (reverse items)
  (define (iter items res)
    (if (null? items) res
        (iter (cdr items) (cons (car items) res))))
  (iter items '()))