(define (stream-limit s tolerance)
  (let ((pred (stream-car s))
        (succ (stream-car (stream-cdr s))))
    (if (> tolerance (abs (- pred succ)))
        succ
        (stream-limit (stream-cdr s) tolerance)))
