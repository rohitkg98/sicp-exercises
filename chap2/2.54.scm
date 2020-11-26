#lang scheme
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) true)
        ((not (and (pair? l1) (pair? l2))) (eq? l1 l2))
        ((or (null? l1) (null? l2)) false)
        ((equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
        (else false)))