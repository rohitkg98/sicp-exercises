#lang scheme
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-adder as bs c ss)
  ; c is the c-out of the final full-adder
  ; for the c-in of the full adder, we have to calculate the next bit
  (let ((c-in (make-wire)))
    ; at the end, c-in we have to use as 0
    ; and stop calling the ripple-adder
    (if (null? (cdr as))
        (set-signal! c-in 0)
        ; call ripple-adder first to set c-in
        (ripple-adder (cdr as) (cdr bs) c-in (cdr s)))
    ; full-adder is always called
    (full-adder (car as) (car bs) c-in (car s) c)))

; the delay would be n full-adders
; for one full-adder; delay is 2* half-adder + or-gate
; for on half-adder; delay is 2*and-gate + or-gate + inverter, we could consider or-gate in parallel with first and-gate
; therefore, full-adder = 4*and-gate + 3*or-gate + 2*inverter
; therefore, ripple-adder = n*(4*and-gate + 3*or-gate + 2*inverter)