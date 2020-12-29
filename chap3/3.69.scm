(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s1 s2 s3)
  (cons-stream (list (stream-car s1) (stream-car s2) (stream-car s3))
               (interleave (stream-map (lambda (x) (cons (stream-car s1) x))
                                       (stream-cdr (pairs s2 s3)))
                           (triples (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))))

(define integers
  (stream-map (lambda (x) (+ x 1)) integers))

(define pythagorean-triplets
  (stream-filter (lambda (x)
                   (let ((a (car x))
                         (b (cadr x))
                         (c (caddr x)))
                     (= (+ (expt a 2)
                           (expt b 2))
                        (expt c 2))))
                 (triples integers integers integers)))

(define (print-stream n s)
  (if (= n 0) (begin (newline)
                     (display (stream-car s)))
      (begin (newline)
             (display (stream-car s))
             (print-stream (- n 1) (stream-cdr s)))))
