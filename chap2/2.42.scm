#lang scheme
(require racket/trace)
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

(define empty-board
  '())

(define (safe? k positions)
  (let ((h-row (car (car positions)))
        (h-col (cdr (car positions)))
        (tail (cdr positions)))
    (accumulate (lambda (el acc)
                  (and (if (or (= h-row (car el))
                               (= (abs (- h-row (car el)))
                                  (abs (- h-col (cdr el)))))
                           #f
                           #t)
                       acc))
                #t
                tail)))
                       

(define (adjoin-position row column rest-of-queens)
  (cons (cons row column) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))