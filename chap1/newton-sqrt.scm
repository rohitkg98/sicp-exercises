(define (good-enough? guess x)
    (> 0.001 (abs (- (* guess guess) x))))

(define (better-good-enough? guess x)
    (= (improve guess x) guess))

(define (average x y)
    (/ (+ x y) 2.0))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(sqrt-iter 1 2)
