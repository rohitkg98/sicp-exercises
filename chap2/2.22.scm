#lang scheme
(define (for-each proc items)
  (define (loop items)
    (proc (car items))
    (newline)
    (for-each proc (cdr items)))
  (if (null? items)
      true
      (loop items)))

; using cond

(define (for-each-cond proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (newline)
              (for-each proc (cdr items)))))

; using discarded varialble
 (define (for-each-discard procedure items) 
   (define (iter items evaluate) 
     (if (null? items) 
       #t 
       (iter (cdr items) (procedure (car items))))) 
   (iter items #t)) 