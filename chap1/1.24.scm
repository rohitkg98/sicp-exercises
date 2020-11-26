#lang scheme
(#%require (lib "27.ss" "srfi"))
(define (square n) (* n n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (when (prime? n)
    (report-prime (- (current-milliseconds) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (find-three-smallest-from n)
  (define (iter n count)
    (cond ((= count 3) n)
          ((prime? n) (begin (timed-prime-test n)
                       (iter (+ n 1) (+ count 1))))
          (else (iter (+ n 1) count))))
  (iter n 0))

