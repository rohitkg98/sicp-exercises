 #lang scheme
;; we would install three packages, sparse, dense and polynomial
;; similar to how complex number was implemented using polar and rect
;; the first two will hold representations for the term-list, which would be tagged as sparse or dense

(define (install-sparse-term-list)

  (define (adjoin-term term term-list) 
    (if (=zero? (coeff term)) 
        term-list
        (cons (term term-list)))) 

  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  ;; interface to rest of the system 
  (define (tag term-list) (attach-tag 'sparse term-list))

  (put 'adjoin-term 'sparse adjoin-term)

  (put 'first-term '(sparse)
       (lambda (term-list) (first-term term-list)))

  (put 'rest-term '(sparse)
       (lambda (term-list) (tag (rest-terms term-list))))
  'done)
  

(define (install-dense-term-list) 

  (define (adjoin-term term term-list) 
    use-definition-from-2.89)  

  (define (first-term term-list) (car term-list)) 
  (define (rest-terms term-list) (cdr term-list))

  ;; interface to rest of the system 
  (define (tag term-list) (attach-tag 'sparse term-list))

  (put 'adjoin-term 'sparse adjoin-term)

  (put 'first-term '(sparse)
       (lambda (term-list) (first-term term-list)))

  (put 'rest-term '(sparse)
       (lambda (term-list) (tag (rest-terms term-list))))
  'done)

(define empty-sparse-term-list
  (cons 'sparse '()))

(define empty-dense-term-list
  (cons 'dense '()))

(define (empty-termlist? l)
  (null? (contents l)))

(define (adjoin-term term term-list)
  ((get 'adjoin-term (type-tag term-list)) term term-list))
  
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-term term-list) (apply-generic 'rest-term term-list))
  
;; the last package stays same as text, we just change the term calls to be generic
;; also, make-poly will be responsible for initial tagging of term-list
;; adjoin-term can only be used on tagged term-lists
;; empty-tagged-lists are out starting point

;; add-terms and rest of the operations will work as is
;; although, they would always return a dense representation

