#lang racket
(require "parse.rkt")

(provide empty-env extended-env empty-env? extended-env? environment? lookup-env init-env primitive-operators)

(define empty-env (lambda () (list 'empty-env)))

(define extended-env (lambda (syms vals old-env) (list 'extended-env syms vals old-env)))

(define empty-env? (lambda (x)
      (cond
            [(not (pair? x)) #f]
            [else (eq? (car x) 'empty-env)])))

(define extended-env? (lambda (x)
      (cond
            [(not (pair? x)) #f]
            [else (eq? (car x) 'extended-env)])))

(define environment? (lambda (x) (or (empty-env? x) (extended-env? x))))

(define syms (lambda (env)
     (cond
           [(extended-env? env) (cadr env)]
           [else (error 'syms "bad environment")])))


(define vals (lambda (env)
      (cond
            [(extended-env? env) (caddr env)]
            [else (error 'vals "bad environment")])))


(define old-env (lambda (env)
     (cond
           [(extended-env? env) (cadddr env)]
           [else (error 'old-env "bad environment")])))

(define the-empty-env (empty-env))
(define EnvA (extended-env '(x y) '(1 2) the-empty-env))
(define EnvB (extended-env '(x z) '(5 7) EnvA))
(define primitive-operators '(+ - * / add1 sub1 minus list build first rest equals? lt? gt?))
(define init-env (extended-env primitive-operators
   (map box (map new-prim-proc primitive-operators))
   (extended-env '(x y nil True False) (map box '(23 45 () #t #f)) the-empty-env)))


(define lookup-env
  (lambda (env sym)
    (cond
      [(empty-env? env)(error 'apply-env "No binding for ~s" sym)]
      [(null? (syms env))(lookup-env (old-env env)sym)]
      [(eq? (car (syms env)) sym)(car (vals env))]
      [else (lookup-env (extended-env (cdr (syms env))(cdr (vals env))(old-env env))sym)])))