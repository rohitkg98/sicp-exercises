#lang scheme
(define (make-point x y)
  (cons x y))

(define x-point car)

(define y-point cdr)

; A rectangle can be represeted using two points
; take two points p1 p2 from endpoints of a diagonal
; then the 4 points of rectangle would be
; c1 = p1.x, p1.y; c2 = p1.x, p2.y; c3 = p2.x p2.y; c4 = p2.x, p21.y;
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define corner-1 car)
(define corner-2 cdr)

(define (length rect)
  (abs (- (x-point (corner-1 rect))
          (x-point (corner-2 rect)))))

(define (breadth rect)
  (abs (- (y-point (corner-1 rect))
          (y-point (corner-2 rect)))))

(define (area rect)
  (* (length rect)
     (breadth rect)))

(define (perimeter rect)
  (* (+ (length rect) (breadth rect))
     2))

(define p1 (make-point -4 3))
(define p2 (make-point 4 -3))

(define rect (make-rectangle p1 p2))
  