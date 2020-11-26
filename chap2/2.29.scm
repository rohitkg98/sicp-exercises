#lang scheme
(define (make-branch length structure)
  ; structure can be number or mobile
  (list length structure))

(define branch-length car)

(define branch-structure cadr)

(define (make-mobile left right)
  (list left right))

(define left-branch car)

(define right-branch cadr)

(define (total-weight mobile)
  (if (not (pair? mobile)) mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and (= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

;; if the constructors are changed, the selectors will be changed from cadr to cdr

(define mobile
  (make-mobile (make-branch 5 5)
               (make-branch 3
                            (make-mobile (make-branch 2 4)
                                         (make-branch 2 6)))))

(define balanced-mobile
    (make-mobile (make-branch 5 12)
               (make-branch 5
                            (make-mobile (make-branch 2 4)
                                         (make-branch 1 8)))))
