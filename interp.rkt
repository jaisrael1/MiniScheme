;The interpreter evaluates expressions and applies procedures as it walks through the parse tree.

#lang racket
(require "parse.rkt")
(require "env.rkt")
(provide eval-exp apply-proc)

(define eval-exp 
        (lambda (tree env) 
               (cond
                      [(lit-exp? tree) (LitValue tree)]
                      [(var-ref? tree) (unbox(lookup-env env (Symbol tree)))]
                      [(lambda-exp? tree) (new-closure (get-closure-params tree)(get-closure-body tree)env)]
                      [(app-exp? tree) (apply-proc (eval-exp (Operator tree) env)(map (lambda(node) (eval-exp node env))(arg-values tree)))]
                      [(if-exp? tree) (if(or(eq? #f (eval-exp(get-cond tree)env))(eq? 0 (eval-exp(get-cond tree)env)))
                                         (eval-exp(get-false tree)env)(eval-exp(get-true tree)env))]
                      [(let-exp? tree) (eval-exp(get-body tree)(extended-env(map Symbol(get-symbols tree))
                                                                            (map box(map (lambda (node) (eval-exp node env))(get-expressions tree)))env))]
                      [(assign-exp? tree) (set-box! (lookup-env env (get-var tree))(eval-exp (get-val tree)env))]
                      [(begin-exp? tree) (last(map (lambda(node) (eval-exp node env)) (begin-expressions tree)))]
                      
                                                               
                      (else (error 'eval-exp  "Invalid tree: ~s" tree)))))

(define apply-proc (lambda (p arg-values)
       (cond
              [ (prim-proc? p) (apply-primitive-op (Operator p) arg-values)]
              [ (closure? p) (eval-exp (get-closure-body p)(extended-env (get-closure-params p) (map box arg-values )(get-closure-env p)))]
                (else (error 'apply-proc "Bad procedure: ~s" p)))))
