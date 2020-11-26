#lang scheme

(define (variable? x) (symbol? x))

(define same-variable? eq?)

(define (=number? x num)
  (and (number? x) (= x num)))

; sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
  

(define (addend sum)
  (cadr sum))

(define (augend sum)
  (foldr make-sum 0 (cddr sum)))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

; product
(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (multiplier product)
  (cadr product))

(define (multiplicand product)
  (foldr make-product 1 (cddr product)))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

; 2.56
; exponentiation

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))

(define base cadr)

(define exponent caddr)

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))