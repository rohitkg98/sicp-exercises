#lang scheme
; An overview of how a real-world library would abstract details
; We wrap cons in make-point as a way to document that
; first argument is x, second is y
(define (make-point x y)
  (cons x y))

; x-point and y-point can be aliased to car and cdr because we don't lose any info
(define x-point car)

(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display y-point p)
  (display ")"))

; Abstracting out scalar operations on points with numbers
(define (apply-scalar-op op p val)
  (make-point (op (x-point p) val)
              (op (y-point p) val)))

; Building scalar ops using our abstraction
(define (divide-point p divisor)
  (apply-scalar-op / p divisor))

; Abstracting out application of scalar operation b/w points
(define (apply-scalar-op-on-points op p1 p2)
  (make-point (op (x-point p1) (x-point p2))
              (op (y-point p1) (y-point p2))))

; Building scalar ops using our abstraction
(define (add-points p1 p2)
  (apply-scalar-op-on-points + p1 p2))

; Again, maintaining name of the arguments here
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define start-segment car)

(define end-segment cdr)

; Defining midpoint using our existing abstraction
(define (midpoint-segment segment)
  (divide-point (add-points (start-segment segment)
                            (end-segment segment))
                2.0))
  