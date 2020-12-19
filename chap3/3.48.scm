#lang scheme
(define make-account-and-serializer
  (let ((serial 0))
    (lambda (balance)
      (set! serial (+ serial 1))
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'serializer) balance-serializer)
                ((eq? m 'serial) serial)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        dispatch))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((a1-s (account1 'serial))
        (a2-s (account2 'serial)))
    (let ((first-serializer (if (< a1-s a2-s
                                   (account1 'serializer)
                                   (account2 'serializer))))
          (second-serializer (if (< a1-s a2-s
                                   (account2 'serializer)
                                   (account1 'serializer)))))
      ((second-serializer (first-serializer exchange))
       account1
       account2))))