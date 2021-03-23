;; request stream, generate 5 random and then reset
(define (rand-requests count)
  (let ((static-count count))
    (define (iter new-count)
      (if (not (eq? new-count 0))
          (cons-stream 'update (iter (- new-count 1)))
          (cons-stream 'reset (iter static-count)))))
  (iter count))

(define (random-numbers reset-stream)
  (cons-stream random-init
               (stream-map (lambda (x op)
                             (cond ((eq? 'update op) (random-update x))
                                   ((eq? 'reset op) random-init)))
                           reset-stream
                           (random-numbers (stream-cdr reset-stream)))))
