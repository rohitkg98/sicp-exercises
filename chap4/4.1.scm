#lang scheme
(define (list-of-values-ltr exps env)
  (let ((first-eval (eval (first-operand exps) env)))
    (if (no-operands? exps)
        '()
        (cons first-eval
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-rtl exps env)
  (let ((rest-eval (list-of-values (rest-operands exps) env)))
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              rest-eval))))
