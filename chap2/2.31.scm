#lang scheme
(define (tree-map proc tree)
  (map (lambda (x)
         (if (pair? x) (tree-map proc x)
             (proc x)))
       tree))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))