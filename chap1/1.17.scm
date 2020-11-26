#lang scheme
(define (double n) (+ n n))

(define (halve n) (/ n 2))

; recursive
(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

; iterative
(define (*-iter a b)
  (define (iter a b odd-add)
    (cond ((= b 0) odd-add)
          ((even? b) (iter (double a) (halve b) odd-add))
          (else (iter a (- b 1) (+ odd-add a)))))
  (iter a b 0))
