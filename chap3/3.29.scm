#lang scheme
(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (not-output (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 not-output)
    (inverter not-output output)
    ; the first two inverter delays should be parallel
    ; the and-gate delay and then the inverter delay
    ; total delay ends up as and-gate-delay + 2*inverter-delay
  'ok))

