; recursive implementation
(define (f n)
  (cond ((<= n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

; linear implementation
(define (f-1 n)
  (define (calc n1 n2 n3)
    (+ n1 (* 2 n2) (* 3 n3)))
  (define (f-iter n-1 n-2 n-3 count)
    (if (= n count)
        (calc n-1 n-2 n-3)
        (f-iter (calc n-1 n-2 n-3) n-1 n-2 (+ 1 count))))
  (cond ((<= n 3) 3)
        (else (f-iter 3 2 1 4))))

