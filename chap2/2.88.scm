#lang scheme
(define (negative num)
  (apply-generic 'negative num))

(define (install-zero-package)
  ;; check if scheme-number zero
  (put 'negative '(scheme-number)
       (lambda (x) (- x)))
  ;; rational
  (put 'negative '(rational)
       (lambda (x)
         (make-rational (- (numer x))
                        (- (denom x)))))

  ;; complex
  (put 'negative '(complex)
       (lambda (x)
         (make-complex (- (real-part x))
                                 (- (imag-part x)))))

  (define (negative-term term)
    (make-term (negative (coeff term))
               (order term)))
  
  (put 'negative '(polynomial)
       (lambda (poly)
         (make-polynomial (variable poly)
                          (map negative-term
                               (term-list poly)))))

  (define (sub-terms L1 L2)
    (add-terms L1 (map negative-term L2)))
      
  (put '=zero? '(polynomial) =zero-poly?))