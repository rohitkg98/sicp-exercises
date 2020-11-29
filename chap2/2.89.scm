#lang scheme
(define (cons-n-zeroes n acc)
  (if (= n 0) acc
      (cons 0 (cons-n-zeroes (- n 1) acc))))

;; assuming they'll be appended sequentially
(define (adjoin-term term term-list)
  (cons (coeff term) term-list))

;; without the prior assumption
(define (adjoin-term-non-seq term term-list)
  (let ((max (- (length term-list) 1))
        (term-ord (order term))
        (term-coeff (coeff term)))
    (cond ((> term-ord max) ;; if term to add has higher order, then add zeroes b/w term to add and head
           (cons term-coeff (cons-n-zeroes (- term-ord max) term-list)))
          ((< term-ord max) ;; if term to add has lower order, keep traversing down until same is found
           (cons (car term-list) (adjoin-term-non-seq term (cdr term-list))))
          ((= term-ord max) ;; if term to add has same order, add to the coeff
           (cons (add (car term-list) term-coeff) term-list)))))
          
  
(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (- (length term-list) 1)
                                          (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
