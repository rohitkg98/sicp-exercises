#lang scheme
(define (call-the-cops)
  (error "Cops have been called!"))

(define (make-account balance password)
  (let ((inc-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch given-pass m)
      (if (eq? given-pass password)
          (begin
            (set! inc-count 0)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
          (begin
            (set! inc-count (+ inc-count 1))
            (if (> inc-count 7)
                (call-the-cops)
                (error "Incorrect Password")))))
    dispatch))

(define peter-acc (make-account 100 'secret-password))
((peter-acc 'secret-password 'withdraw) 40)

;; 3.7
(define (make-joint acc acc-pass joint-pass)
  (lambda (password m)
    (if (eq? password joint-pass)
        (acc acc-pass m)
        (error "Incorrect joint password"))))

(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud))
