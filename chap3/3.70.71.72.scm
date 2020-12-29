(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  ;(display-all "s1: " s1car ", " (weight s1car) ", s2: " s2car ", " (weight s2car))
                  (cons-stream s1car (merge-weighted
                                      weight (stream-cdr s1) s2)))
                 ((> (weight s1car) (weight s2car))
                   ;(display-all "s1: " s1car ", " (weight s1car) ", s2: " s2car ", " (weight s2car))
                  (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2))))
                 (else
                  ;(display-all "s1: " s1car ", " (weight s1car) ", s2: " s2car ", " (weight s2car))
                  (cons-stream s1car
                               (cons-stream s2car (merge-weighted weight (stream-cdr s1)
                                      (stream-cdr s2))))))))))


(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define integers
  (cons-stream 0 (stream-map (lambda (x) (+ x 1))
                             integers)))

(define (ordered-pairs)
  (weighted-pairs (lambda (x) (+ (car x)
                                 (cadr x)))
                  integers
                  integers))

(define custom-pairs
  (weighted-pairs (lambda (x)
                    (let ((i (car x))
                          (j (cadr x)))
                      (+ (* 2 i)
                         (* 3 j)
                         (* 5 i j))))
                  integers
                  integers))

(define (cube pair)
  (+ (expt (car pair) 3)
     (expt (cadr pair) 3)))

(define cube-weighted-pairs
  (weighted-pairs cube
                  integers
                  integers))

(define (search-ramanujan-numbers pairs)
  (let ((first (stream-car pairs))
        (second (stream-ref pairs 1)))
    (if (= (cube first)
           (cube second))
        (cons-stream (cube first) (search-ramanujan-numbers (stream-cdr pairs)))
        (search-ramanujan-numbers (stream-cdr pairs)))))

(define ramanujan-numbers
  (search-ramanujan-numbers cube-weighted-pairs))
; 1729
; 4104
; 13832
; 20683
; 32832

(define (square pair)
  (+ (expt (car pair) 2)
     (expt (cadr pair) 2)))

(define square-weighted-pairs
  (weighted-pairs square
                  (stream-cdr integers) ; only natural numbers
                  (stream-cdr integers)))

(define (search-square-sums pairs)
  (let ((first (stream-car pairs))
        (second (stream-ref pairs 1))
        (third (stream-ref pairs 2)))
    (if (= (square first)
           (square second)
           (square third))
        (cons-stream (square first) (search-square-sums (stream-cdr pairs)))
        (search-square-sums (stream-cdr pairs)))))

(define square-sums
  (search-square-sums square-weighted-pairs))

(define (print-stream n s)
  (if (= n 0)
      (begin (newline)
             (display (stream-car s)))
      (begin (newline)
             (display (stream-car s))
             (print-stream (- n 1) (stream-cdr s)))))

(define (display-all . vs)
  (for-each display vs)
  (newline))
