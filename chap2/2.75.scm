#lang scheme
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else (error "Unknown op -- MAKE-FORM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))