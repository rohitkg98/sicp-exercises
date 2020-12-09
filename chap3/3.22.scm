#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty?) (null? front-ptr))

    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))
    
    (define (insert! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))

    (define (delete!)
      (cond ((empty?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (mcdr front-ptr))
             dispatch)))

    (define (print)
      (mfor-each (lambda (x)
              (begin (display x)
                     (display " ")))
            front-ptr))
    
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            ((eq? m 'print) print)
            (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty?)))

(define (front-queue queue)
  ((queue 'front)))

(define (insert-queue! queue item)
  ((queue 'insert!) item))

(define (delete-queue! queue)
  ((queue 'delete!)))

(define (print-queue queue)
  ((queue 'print)))

