#lang scheme
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (addend s) (car s))
(define (augend s)
  (foldr make-sum 0 (cdr s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier p) (car p))
(define (multiplicand p)
  (foldr make-product 1 (cdr  p)))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))

(define base car)

(define exponent cadr)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
; a
; number and same-variable can't be in dispatch
; due to not having same arguments as rest

(define (install-deriv-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (deriv-mul exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (define (deriv-exponentiation exp var)
    (make-product
     (make-product (exponent exp)
                   (make-exponentiation
                    (base exp)
                    (make-sum (exponent exp) -1)))
     (deriv (base exp) var)))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-mul)
  (put 'deriv '** deriv-exponentiation))

(install-deriv-package)
