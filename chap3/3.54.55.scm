(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

; 3.55
(define (partial-sums s)
  (define ps
    ; the first element has to be as is
    (add-streams s (cons-stream 0 ps))
  ps)
