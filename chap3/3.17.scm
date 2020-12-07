#lang scheme
(define (count-pairs x)
  (let ((counted '()))
    (define (iter x)
      (if (or (not (pair? x))
              (memq x counted))
          0
          (let ((car-count (iter (car x))))
            ; add to counted, once it gets counted
            (begin (set! counted (cons (car x) counted))
                   (+ car-count
                      (iter (cdr x))
                      1)))
          ))
  (iter x)))