#lang scheme
(define (front-ptr queue) (car queue))

(define (print-queue queue)
  (display (foldl (lambda (x acc)
                    (begin (display x)
                           (display " ")
                           acc))
                  ""
                  (front-ptr queue))))