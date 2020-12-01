#lang scheme
(define f
  (let ((count 0)
        (prev-count 0))
    (lambda (x)
      (begin (set! prev-count count)
             (set! count x)
             prev-count))))
;; (+ (f 0) (f 1))

;; (+ (f 1) (f 0))

(define simpler-f 
  (let ((count 1))
    (lambda (x)
      (set! count (* count x))
      count)))

;; (+ (simpler-f 0) (simpler-f 1))

;; (+ (simpler-f 1) (simpler-f 0))