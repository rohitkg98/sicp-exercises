#lang scheme
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ 1 start) end))))

(define (unique-triple-pairs n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval (+ j 1) n)))
                  (enumerate-interval (+ i 1) (- n 1))))
           (enumerate-interval 1 (- n 2))))

(define (triple-pair-sum pair)
  (append pair (+ (car pair) (cadr pair) (caddr pair))))

(define (triple-pair-sum-equals n s)
  (filter (lambda (i) (= (cdddr i) s))
          (map triple-pair-sum
               (unique-triple-pairs n))))