#lang scheme

(define (install-zero-package)
  ;; check if scheme-number zero
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ;; rational
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))

  ;; complex
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  
  ;; to check a polymial is zero,
  ;; all it's term's coeff should be zero, which will be dispatched on type
  ;; order can be anything
  (define (=zero?-term term)
    (if (=zero? (coeff term))
        #t
        #f))
  (define (=zero?-poly poly)
    (foldr (lambda (term acc)
             (and (=zero?-term term) acc))
           #t
           (term-list poly)))
      
  (put '=zero? '(polynomial) =zero-poly?))
       