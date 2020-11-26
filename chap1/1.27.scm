#lang scheme
(define (square n) (* n n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-full n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter a)
    (cond ((= n a) #t)
          ((try-it a) (iter (+ 1 a)))
          (else #f)))
  (iter 1))


