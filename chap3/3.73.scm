(define (add-streams . s)
  (apply stream-map + s))

(define (scale-stream s c)
  (stream-map (lambda (x) (* x c)) s))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt))))
  int)

(define (RC R C dt)
  (define (vs i v0)
    (cons-stream v0 (add-streams (scale-stream i R)
                                 (integral (scale-stream i (/ 1 C)) v0 dt))))
  vs)
