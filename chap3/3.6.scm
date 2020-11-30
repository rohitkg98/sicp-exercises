#lang scheme
(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (set! x (rand-update x))
             x)
            ((eq? op 'reset)
             (lambda (val)
               (set! x val)))))))