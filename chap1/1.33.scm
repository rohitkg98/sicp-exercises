#lang scheme
(require racket/trace)
; The Miller-Rabin Prime test
(define (square x) (* x x)) 
  
(define (miller-rabin-expmod base exp m) 
  (define (squaremod-with-check x) 
    (define (check-nontrivial-sqrt1 x square) 
      (if (and (= square 1) 
               (not (= x 1)) 
               (not (= x (- m 1)))) 
          0 
          square)) 
    (check-nontrivial-sqrt1 x (remainder (square x) m))) 
  (cond ((= exp 0) 1) 
        ((even? exp) (squaremod-with-check 
                      (miller-rabin-expmod base (/ exp 2) m))) 
        (else 
         (remainder (* base (miller-rabin-expmod base (- exp 1) m)) 
                    m)))) 
  
(define (miller-rabin-test n) 
  (define (try-it a) 
    (define (check-it x) 
      (and (not (= x 0)) (= x 1))) 
    (check-it (miller-rabin-expmod a (- n 1) n))) 
  (try-it (+ 1 (random (- n 1)))))
  
(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((miller-rabin-test n) (fast-prime? n (- times 1))) 
        (else false))) 
  
(define (prime? n)
  (if (= n 1) #f
  (fast-prime? n 100)))

; Till here

; GCD definition

(define (gcd a b)
  (if (= 0 (remainder a b)) b
      (gcd b (remainder a b))))

; Till here

(define (filtered-accumulate combiner filter? null-value term a next b)
  (cond ((> a b) null-value)
        ((filter? a) (filtered-accumulate combiner filter? null-value term (next a) next b))
        (else (combiner (term a)
                        (filtered-accumulate combiner filter? null-value term (next a) next b)))))

(define (filtered-accumulate-iter combiner filter? null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter? a) (iter (next a) result))
          (else (iter (next a) (combiner result
                                         (term a))))))
  (iter a null-value))

(define (filtered-sum filter? term a next b)
  (filtered-accumulate + filter? 0 term a next b))

(define (inc x) (+ 1 x))

(define (sum-of-prime-squares a b)
  (define (not-prime? x) (not (prime? x)))
  (filtered-sum not-prime? square a inc b))

(define (filtered-product filter? term a next b)
  (filtered-accumulate * filter? 1 term a next b))

(define (product-of-relative-prime n)
  (define (not-relative-prime? x)
    (not (= 1 (gcd x n))))
  (trace not-relative-prime?)
  (filtered-product not-relative-prime? identity 1 inc n))