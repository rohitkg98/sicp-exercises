#lang scheme
(define x (list (list 1 2) (list 3 4)))

(define nil '())

(define (fringe items)
  (if (not (pair? items))
      (list items)
      (let ((end (cdr items)))
        (if (null? end) (fringe (car items))
            (append (fringe (car items))
                    (fringe end))))))

(define (fringe-iter-reverse items)
  (define (iter first rest res)
    (cond ((and (null? first) (null? rest)) res)
          ((null? first) (iter (car rest) (cdr rest) res))
          ((pair? first) (iter (car first) (cons (cdr first) rest) res))
          (else (iter (car rest) (cdr rest) (cons first res)))))
  (iter '() items '()))

(define (fringe-iter items)
  (define (iter items res)
    (cond ((null? items) res)
          ((not (pair? items)) (cons items res))
          (else (iter (car items) (iter (cdr items) res)))))
  (iter items '()))
    

(fringe x)

(fringe (list x x))