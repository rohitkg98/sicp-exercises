(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

; 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay dy2) dy0 dt))
  (define dy2 (stream-map + (scale-stream dy a) (scale-stream y b)))
  y)

; 3.79
(define (generalized-solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay dy2) dy0 dt))
  (define dy2 (stream-map f dy y))
  y)
