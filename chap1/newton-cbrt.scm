(define (good-enough? guess x)
    (> 0.001 (abs (- (* guess guess) x))))

(define (better-good-enough? guess x)
    (= (improve guess x) guess))

(define (average-3 x y)
    (/ (+ x y) 3.0))

(define (improve guess x)
    (average-3 (/ x (* guess guess)) (* 2 guess)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(sqrt-iter 1 2)

(define (A x y)
  (cond ((= y 0) 0)
        ))
