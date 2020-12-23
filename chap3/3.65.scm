(define (log-2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (log-2-summands (+ n 1)))))

(define log-2
  (partial-sums (log-2-summands 1)))
