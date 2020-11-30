#lang scheme
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
   (= (gcd (random 1 100000) (random 1 100000)) 1))

(define (area x1 x2 y1 y2)
  (* (abs (- x1 x2))
     (abs (- y1 y2))))

;; assume x1 < x2, y1 < y2
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P x1 x2 y1 y2))
  (* (area x1 x2 y1 y2) (monte-carlo trials experiment)))

(define (unit-circle centre-x centre-y)
  (lambda (x1 x2 y1 y2)
     (let ((x (random x1 x2))
           (y (random y1 y2)))
       (>= 1 (+ (sqr (- x centre-x))
                (sqr (- y centre-y)))))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define pi-approx
  (* 1.0
     (estimate-integral (unit-circle 1 1) 0 2 0 2 100000)))
;; on drracket pi approximation turns out to ~3
;; probably due to the nature of psuedo-random generator not being completely uniform