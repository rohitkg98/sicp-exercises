#lang scheme
(define (smallest-divisor n)
  (define (next divisor)
    (if (= 2 divisor)
        3
        (+ 2 divisor)))
  (define (find-divisor divisor)
    (cond ((< n (* divisor divisor)) n)
          ((= 0 (remainder n divisor)) divisor)
          (else (find-divisor (next divisor)))))
  (find-divisor 2))
(define (prime? n)
  (= n (smallest-divisor n)))

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

