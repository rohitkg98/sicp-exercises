(define (area x1 x2 y1 y2)
  (* (abs (- x1 x2))
     (abs (- y1 y2))))

;; assume x1 < x2, y1 < y2
(define (estimate-integral P x1 x2 y1 y2)
  (define (experiment)
    (P x1 x2 y1 y2))
  (stream-map (lambda (x) (* x (area x1 x2 y1 y2) 1.0)) (monte-carlo experiment)))

(define (unit-circle centre-x centre-y)
  (lambda (x1 x2 y1 y2)
     (let ((x (+ x1 (random (- x2 x1))))
           (y (+ y1 (random (- y2 y1)))))
       (>= 1 (+ (square (- x centre-x))
                (square (- y centre-y)))))))

;; monte-carlo would be a stream of trials
(define (monte-carlo experiment)
  (define (iter trials trials-passed)
    (cons-stream (/ trials-passed trials)
                 (iter (+ trials 1)
                       (+ trials-passed (if (experiment) 1 0)))))
  (iter (if (experiment) 1 0) 1))

(define pi-approx
  (estimate-integral (unit-circle 1 1) 0 2 0 2))
