; averaging the stream with itself
(define (smooth input-stream)
  (stream-map (lambda (x y) (/ (+ x y) 2))
              input-stream
              (stream-cdr input-stream)))

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector input-stream (cons-stream 0 input-stream)))

(define (smooth-zero-crossings input-stream)
  (make-zero-crossings (smooth input-stream)))
