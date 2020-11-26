#lang scheme
(define nil '())

(define (square-list-pure items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list-pure (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))
