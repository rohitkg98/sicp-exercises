#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)

(define (entry tree) (mcar tree))

(define (left-branch tree) (mcar (mcdr tree)))
(define (set-left! tree val) (set-car! (mcdr tree) (make-tree val '() '())))

(define (right-branch tree) (mcar (mcdr (mcdr tree))))
(define (set-right! tree val) (set-car! (mcdr (mcdr tree)) (make-tree val '() '())))

(define (make-tree entry left right)
  (mlist entry left right))

; A binary tree, would insert and search in a different manner
; that changes the implementation of our assoc
(define (assoc key records)
  (cond ((null? records) false)
        ((= key (mcar (mcar records))) (mcar records))
        ((< key (mcar (mcar records))) (assoc key (left-branch records)))
        (else (assoc key (right-branch records)))))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (insert! key value table)
  (let ((parent (mcdr table)))
    (define (add-node node)
      (let ((left (left-branch parent))
            (right (right-branch parent))
            (parent-val (mcar (entry parent))))
            (cond ((= key parent-val)
                   (set-cdr! (entry parent) value))
                  ((< key parent-val)
                   (if (null? (left-branch parent))
                              (set-left! parent node)
                       (begin
                        (set! parent (left-branch parent))
                        (add-node node))))
                  ((> key parent-val)
                   (if (null? (right-branch parent))
                              (set-right! parent node)
                       (begin
                        (set! parent (right-branch parent))
                        (add-node node)))))))
    (let ((record (assoc key (mcdr table))))
      (if record
          (set-cdr! record value)
          (cond ((null? (mcdr table))
                 (set-cdr! table
                           (make-tree (mcons key value) '() '())))
                (else (add-node (mcons key value))))))
  'ok))

(define (make-table)
  (mlist '*table*))

(define table (make-table))