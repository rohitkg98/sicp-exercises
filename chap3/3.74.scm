; this doesn't work out according to book output
; it does work out if we consider the fact that the book never shows start of the stream
(define zero-crossings
  (sign-change-detector sense-data (stream-cdr sense-data)))

; will give according to book output
(define zero-crossings
  (sign-change-detector sense-data (cons-stream 0 sense-data)))
