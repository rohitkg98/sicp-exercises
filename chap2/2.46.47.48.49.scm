#lang scheme
(define (make-vect x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
                (xcor-vect b))
             (+ (ycor-vect a)
                (ycor-vect b))))


(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a)
                (ycor-vect b))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v)
                s)
             (* (ycor-vect v)
                s)))

(define x (make-vect 1 4))

(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame-list car)

(define edge1-frame-list cadr)

(define edge2-frame-list caddr)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame cddr)

; 2.48
; As the vectors are from origin, we can directly use them as points
(define (make-segment till-start till-end)
  (cons till-start till-end))

(define start-segment car)
(define end-segment cdr)

; 2.49

(define (draw-line x y) 1)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; a
(define (frame-outline)
  (let ((tl (make-vect 0 1))
        (bl (make-vect 0 0))
        (tr (make-vect 1 1))
        (br (make-vect 1 0)))
    (list (make-segment bl tl)
          (make-segment bl br)
          (make-segment tl tr)
          (make-segment br tr))))

; b
(define diagonals
  (let ((tl (make-vect 0 1))
        (bl (make-vect 0 0))
        (tr (make-vect 1 1))
        (br (make-vect 1 0)))
    (list (make-segment bl tr)
          (make-segment tl br))))

; c
(define diamond
  (let ((down (make-vect 0.5 0))
        (left (make-vect 0 0.5))
        (right (make-vect 1 0.5))
        (up (make-vect 0.5 1)))
    (list (make-segment left up)
          (make-segment left down)
          (make-segment right up)
          (make-segment right down))))

; 2.50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0) 
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))

(define (below-rot painter1 painter2)
  (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))

; 2.52
; a


