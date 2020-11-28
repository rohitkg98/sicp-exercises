#lang scheme
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define *coercion-table* (make-hash))

(define (put-coercion op type proc)
  (hash-set! *coercion-table* (list op type) proc))

(define (get-coercion op type)
  (hash-ref *coercion-table* (list op type) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

;;;;
;;;; MAIN LOGIC FOR 2.82
;;;;           


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; for n args, map over type-tags
          (let ((higher-type ((get-coercion 'get-higher-type 'number) type-tags)))
            (let ((raised-types (map (lambda (x)
                                       (raise-to-given x higher-type))
                                     args)))
            (if (not (equal? args raised-types))
                (apply apply-generic op
                       raised-types)
                (error "No method for these types"
                       (list op type-tags)))))))))

;;;;
;;;; TILL HERE
;;;;

;; install scheme number and complex for testing
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
;; 2.79
(define (equ? x y) (apply-generic 'equ? x y))
;; 2.80
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))

(define (square x) (* x x))

;; scheme-number package
(define (install-scheme-number-package)
  (define (tag x)
    (cons 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; 2.79
  (put 'equ? '(scheme-number scheme-number)
        (lambda (x y) (= x y)))
  ;; 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)
(install-scheme-number-package)


;; rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; 2.79
  (define (equ?-rat x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  ;; interface to rest of the system
  (define (tag x) (cons 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;; 2.79
  (put 'equ? '(rational rational)
       equ?-rat)
  ;; 2.80
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))



;; rect-package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; 2.79
  (define (equ?-rect z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;; interface to the rest of the system
  (define (tag x) (cons 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; 2.79
  (put 'equ? '(rectangular rectangular)
       equ?-rect)
  ;; 2.80
  (put '=zero? '(rectangular)
       (lambda (x) (and (= 0 (real-part x))
                        (= 0 (imag-part x)))))
  'done)
(install-rectangular-package)

;; polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; 2.79
  (define (equ?-polar z1 z2)
    (and (= (magnitude z1) (magnitude z2))
         (= (angle z1) (angle z2))))
  ;; interface to the rest of the system
  (define (tag x) (cons 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; 2.79
  (put 'equ? '(polar polar) equ?-polar)
  ;; 2.80
  (put '=zero? '(polar)
       (lambda (x) (= 0 (magnitude x))))
  'done)
(install-polar-package)


;; complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; complex package needs to be modified to install additional op
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ;; 2.79
  (define (equ?-complex z1 z2)
    (apply-generic 'equ? z1 z2))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (cons 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; 2.79
  (put 'equ? '(complex complex) equ?-complex)
  ;; 2.80
  (put '=zero? '(complex)
       (lambda (x) (apply-generic '=zero? x)))
  ;; magnitude for complex is just dispatching
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))


(define (install-type-conversion-package)
  (define (scheme-number->rational int)
    (make-rational int 1))

  (define (rational->complex r)
    (make-complex-from-real-imag r 0))

  ;; instead of this hard coding numerical levels workout better
  ;; check scheme wiki for numerical levels solutions
  ;; reminds me of make-denomination tackled earlier
  ;; the levels version is implemented in 2.83
  (define (get-higher-type types)
    (foldl (lambda (type acc)
             (cond ((eq? type 'complex) type)
                   ((eq? type 'rational)
                    (if (eq? acc 'complex)
                        acc
                        type))
                   ((eq? type 'scheme-number)
                    (if (or (eq? acc 'complex)
                            (eq? acc 'rational))
                        acc
                        type))
                   (else (error "The type does not exist in the tower -- " type))))
           'scheme-number
           types))

  (define (level type)
   (cond ((eq? type 'scheme-number) 0) 
         ((eq? type 'rational) 1) 
         ((eq? type 'real) 2) 
         ((eq? type 'complex) 3) 
         (else (error "Invalid type: LEVEL" type)))) 

  (put-coercion 'scheme-number 'rational
                scheme-number->rational)
  (put-coercion 'rational 'complex
                rational->complex)
  (put-coercion 'get-higher-type 'number
                get-higher-type)

  (put 'raise '(scheme-number)
       scheme-number->rational)
  (put 'raise '(rational)
       rational->complex))
(install-type-conversion-package)

(define (raise-to-given sub max)
    (if (eq? (type-tag sub) max)
        sub
        (raise-to-given (raise sub) max)))

(add (cons 'scheme-number 1) (make-rational 10 2))

