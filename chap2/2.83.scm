#lang scheme
;;;;
;;;; Setup
;;;;

;; op and coercion table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define *coercion-table* (make-hash))

(define (put-coercion from to proc)
  (hash-set! *coercion-table* (list from to) proc))

(define (get-coercion from to)
  (hash-ref *coercion-table* (list from to) #f))
;;

(define (no-method types)
  (error "No method for the following -- "))


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (make-complex-from-real-imag r i)
  (attach-tag 'complex (cons r i)))

(define (make-real frac)
  (attach-tag 'real frac))

(define (make-rational n d)
  (let ((factor (gcd n d)))
    (attach-tag 'rational (cons (/ n factor) (/ d factor)))))

(define (make-integer i)
  (attach-tag 'integer i))

;;;;
;;;; End
;;;;

(define (install-type-conversion-package)
  (define (integer->rational int)
    (make-rational int 1))
  
  (define (rational->real rat)
    (cons 'real (/ (car rat) (cdr rat))))

  (define (real->complex r)
    (make-complex-from-real-imag r 0))

  (define (level type) 
    (cond ((eq? type 'integer) 0)
          ((eq? type 'rational) 1)
          ((eq? type 'real) 2)
          ((eq? type 'complex) 3)
          (else (error "Invalid type: LEVEL" type))))

  (define (level-to-type level)
    (cond ((= 0 level) 'integer)
          ((= 1 level) 'rational)
          ((= 2 level) 'real)
          ((= 3 level) 'complex)
          (else (error "Invalid type: LEVEL" level))))

  (put-coercion 'integer 'rational
                integer->rational)
  (put-coercion 'rational 'real
                rational->real)
  (put-coercion 'real 'complex
                real->complex)

  
  (put 'project '(rational)
       (lambda (r) 
         (make-integer
          (floor (/ (car r) (cdr r))))))

  ;; taken from eli bendersky
  (put 'project '(real)
       (lambda (r) 
         (let ((scheme-rat 
                (rationalize 
                 (inexact->exact r) 1/100)))
           (make-rational
            (numerator scheme-rat)
            (denominator scheme-rat)))))

  (put 'project '(complex)
       (lambda (c) (make-real (car c))))

  (put 'raise '(integer)
       integer->rational)
  (put 'raise '(rational)
       rational->real)
  (put 'raise '(real)
       real->complex)

  ;; in a realistic system, we would have a type for types
  ;; in this system, we could insert level for every type, but too cumbersome
  (put 'level 'number
       level)
  (put 'level-to-type 'number
       level-to-type)
  )
(install-type-conversion-package)

(define (level type)
  ((get 'level 'number) type))

(define (level-to-type level)
  ((get 'level-to-type 'number) level))

(define (find-highest-type types)
  (foldr (lambda (type highest-type)
           (if (> (level type) (level highest-type))
               type
               highest-type))
         (car types)
         types))

(define (raise num)
  (apply-generic 'raise num))

(define (raise-to-given data higher)
  (if (eq? (type-tag data) higher)
      data
      (raise-to-given (raise data) higher)))

(define (project num)
          (apply-generic 'project num))

(define (drop num)
  (if (= 0 (level (type-tag num)))
      num
      (let ((dropped (project num)))
        (if (equal? num (raise dropped))
            (drop dropped)
            num))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (eq? op 'raise)
          (apply proc (map contents args))
          (if proc
              (drop (apply proc (map contents args)))
              (let ((highest-type (find-highest-type type-tags)))
                (let ((raised-types (map (lambda (x)
                                           (raise-to-given x highest-type))
                                         args)))
                  (if (not (equal? args raised-types))
                      (apply apply-generic op
                             raised-types)
                      (error "No method for these types"
                             (list op type-tags))))))))))

;; goes through all levels of tower, integer to complex
(raise (raise (raise (cons 'integer 1))))
