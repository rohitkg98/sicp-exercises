#lang scheme

(define (same-parity . ints)
  (let ((parity (remainder (car ints) 2)))
    (define (iter ints)
      (cond ((null? ints) '())
          ((= parity
              (remainder (car ints)
                         2))
           (cons (car ints) (iter (cdr ints))))
          (else (iter (cdr ints)))))
    (iter ints)))