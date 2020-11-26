#lang scheme
(define (cont-frac n-term d-term k)
  (define (iter i)
    (let ((n-i (n-term i))
          (d-i (d-term i)))
      (if (= i k) (/ n-i d-i)
          (/ n-i (+ d-i (iter (+ i 1)))))))
  (iter 1))

(define (cont-frac-iter n-term d-term k)
  (define (iter i result)
    (let ((n-i (n-term i))
          (d-i (d-term i)))
      (if (= i 0) result
          (iter (- i 1) (/ n-i (+ d-i result))))))
  (iter k (/ (n-term k) (d-term k))))

(define (golden k)
  (cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           k))