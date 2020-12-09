#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)

; make element of queue point to previous: (prev, value)
(define (make-el prev val)
  (mcons prev val))
(define (prev-ptr el)
  (mcar el))
(define (value el)
  (mcdr el))

(define (set-prev! el prev-ptr)
  (set-car! el prev-ptr))

(define (set-val! el val)
  (set-cdr! el val))

(define (front-ptr deque) (mcar deque))
(define (rear-ptr deque) (mcdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (mcons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (mcar (front-ptr deque)))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (value (mcar (rear-ptr deque)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (mcons (make-el (rear-ptr deque) item)
                         '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-el '() item)))
    (let ((new-front-ptr (mcons new-pair
                                (front-ptr deque))))
      (cond ((empty-deque? deque)
             (set-front-ptr! deque new-front-ptr)
             (set-rear-ptr! deque new-front-ptr)
             deque)
            (else
             (set-prev! (mcar (front-ptr deque)) new-front-ptr)
             (set-front-ptr! deque new-front-ptr)
             deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        (else
         ; can optionally set prev-ptr to '() for the new front-ptr if it's not null
         (set-front-ptr! deque (mcdr (front-ptr deque)))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        (else
         ;; set as rear-ptr
         (set-rear-ptr! deque (prev-ptr (mcar (rear-ptr dq))))
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque '())
             (set-cdr! (rear-ptr deque) '()))
         deque)))

(define dq (make-deque))