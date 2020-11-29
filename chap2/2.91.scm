#lang scheme
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (let ((new-dividend
                            (add-terms L1
                                       (negative
                                        (mul-terms (adjoin-term
                                                    (make-term new-c new-0)
                                                    the-empty-termlist)
                                                   L2)))))
                       (div-terms new-dividend L2))
                     ))
                (list (adjoin-term (make-term new-c new-0)
                                   (car rest-of-result))
                      (cadr rest-of-result))
                ))))))

(define (div-poly p1 p2)
  (if (not (same-variable? p1 p2))
      (error "Can only divide same variable poly")
      (let ((result (div-terms (term-list p1)
                               (term-list p2))))
        (list (make-poly (variable p1)
                         (car result))
              (make-poly (variable p2)
                         (cadr result))))))
      