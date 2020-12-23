(define (div-series numer denom)
  (let ((numer-cnst (stream-car numer))
        (denom-cnst (stream-car denom)))
    ; scale the streams by the inverse of their cnst to get a unit series
    ; and then scale back to get normal
    (let ((scaled-numer (scale-stream numer (/ 1 numer-cnst)))
          (scaled-denom (scale-denom denom (/ 1 denom-cnst))))
      (if (= denom-cnst 0) (error "DIV-SERIES called with 0 denom-cnst--")
          (let ((result (mul-series scaled-numer (invert-unit-series denom))))
            (scale-stream (scale-stream result denom-cnst) numer-cnst))))))

; tangent definition
(define tangent-series (div-series sine-series cosine-series))
