#lang scheme
(require racket/trace)
(define (cube x) (* x x x))

(define (simpson-integral-without-sum f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* h k))))
  (define (iter count)
    (cond ((= count 0) (y 0))
          ((= count n) (+ (y count)
                          (iter (- count 1))))
          (else (+ (* (if (even? count) 2 4)
                      (y count))
                   (iter (- count 1))))))
  (* (/ h 3.0)
     (iter n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ 1 x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (define y (f (+ a (* h k))))
    (if (or (= k n)
            (= k 0))
        y
        (* (if (even? k) 2 4) y)))
  (* (/ h 3) (sum simpson-term 0 inc n)))
    