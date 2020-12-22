(define (invert-unit-series s)
  (let ((sr (stream-cdr s)))
    (define x
      (cons-stream 1 (stream-map - (mul-series x sr))))))
