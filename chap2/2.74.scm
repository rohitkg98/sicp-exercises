#lang scheme
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (open-file division)
  ; return some divison file)
  ; division file should have div info as first key
  )

(define (make-division div-type employees))

(define (make-employee div-type address salary))
(define (get-employee-div employee))

; attach the tag
(define (attach-tag type-tag content) (cons type-tag content)) 
(define (get-employee div-file name)
  (attach-tag (car div-file) ((get (car div-file) 'get-employee) name)))

(define (get-salary employee)
  ((get (employee-div employee) 'get-salary) employee))

(define (get-record division name)
  (get-employee (open-file divison) name))

(define (find-employee-record name files)
  (cond ((null? files) #f)
        ((not (= #f (get-employee (car files) name)))
         (get-employee (car files) name))
        (else find-employee-record name (cdr files))