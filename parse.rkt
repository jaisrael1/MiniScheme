#lang racket
(provide parse new-lit-exp lit-exp? LitValue new-var-ref var-ref? Symbol new-prim-proc prim-proc?
         Operator new-app-exp app-exp? arg-values apply-primitive-op new-if-exp if-exp? get-cond
         get-true get-false new-let-exp let-exp? get-symbols get-expressions get-body new-lambda-exp
         lambda-exp? new-closure closure? get-closure-params get-closure-body get-closure-env
         new-assign-exp assign-exp? get-var get-val new-begin-exp begin-exp? begin-expressions)


;PARSING
(define parse 
  (lambda (exp)
    (cond
      [(and(pair? exp)(lambda-exp? exp))
       (new-lambda-exp (get-closure-params exp) (parse (get-closure-body exp)))]
      [(and(pair? exp)(let-exp? exp))
       (new-let-exp (map parse(map car(cadr exp)))(map parse(map cadr(cadr exp)))(parse (caddr exp)))]
      [(and(pair? exp)(if-exp? exp))
       (new-if-exp (parse (get-cond exp))(parse (get-true exp))(parse (get-false exp)))]
      [(and(pair? exp)(eq? (car exp)'set!))(new-assign-exp (get-var exp)(parse (get-val exp)))]
      [(and(pair? exp)(eq? (car exp)'begin))(new-begin-exp (map parse(cdr exp)))]
      [(and(pair? exp)(eq? (car exp)'letrec))
       (make-letrec (map parse(map car(cadr exp)))(map parse(map cadr(cadr exp)))(parse (caddr exp)))]
      [(pair? exp)(new-app-exp (parse (car exp))(map parse (cdr exp)))]
      [(number? exp) (new-lit-exp exp)]
      [(not(number? exp)) (new-var-ref exp)]
      [else (error 'parse "Invalid syntax ~s" exp)])))


;LETREC EXPRESSIONS
(define make-letrec
  (lambda (ids vals body)
    (let ([randomsyms (gensyms ids '())])
      (new-let-exp ids (setzeros ids '())
                 (new-let-exp randomsyms vals
                              (new-begin-exp (append(map new-assign-exp (map cadr ids) randomsyms)(list body))))))))


(define setzeros
  (lambda (ids zeros)
    (cond
      [(null? ids)zeros]
      [else (setzeros (cdr ids)(cons (new-lit-exp '0) zeros))])))
(define gensyms
  (lambda (ids syms)
    (cond
      [(null? ids)syms]
      [else (gensyms (cdr ids)(cons (new-var-ref (gensym)) syms))])))


;LITERAL EXPRESSIONS
(define new-lit-exp
  (lambda (exp)
    (list 'lit-exp exp)))
(define lit-exp?
  (lambda (exp)
    (if (eq? (car exp) 'lit-exp) #t #f)))
(define LitValue
  (lambda (exp)
    (cadr exp)))

;VARIABLE REFERENCES
(define new-var-ref
  (lambda (exp)
    (list 'var-ref exp)))
(define var-ref?
  (lambda (exp)
    (if (eq? (car exp) 'var-ref) #t #f)))
(define Symbol
  (lambda (exp)
    (cadr exp)))

;PRIMITIVE PROCEDURES
(define new-prim-proc
  (lambda (exp)
    (list 'prim-proc exp)))
(define prim-proc?
  (lambda (exp)
    (if (eq? (car exp) 'prim-proc) #t #f)))
(define Operator
  (lambda (exp)
    (cadr exp)))

;APPLICATION EXPRESSIONS
(define new-app-exp
  (lambda (operator args)
    (list 'app-exp operator args)))
(define app-exp?
  (lambda (exp)
    (if (eq? (car exp) 'app-exp) #t #f)))
(define arg-values
  (lambda (exp)
    (caddr exp)))
(define apply-primitive-op (lambda (op arg-values)
       (cond
              [(eq? op '+) (+ (car arg-values) (cadr arg-values))]
              [(eq? op '-) (- (car arg-values) (cadr arg-values))]
              [(eq? op '*) (* (car arg-values) (cadr arg-values))]
              [(eq? op '/) (/ (car arg-values) (cadr arg-values))]
              [(eq? op 'add1) (+ 1 (car arg-values))]
              [(eq? op 'sub1) (- (car arg-values) 1 )]
              [(eq? op 'minus) (* -1 (car arg-values))]
              [(eq? op 'list) arg-values]
              [(eq? op 'equals?) (eqv? (car arg-values)(cadr arg-values))]
              [(eq? op 'lt?) (< (car arg-values)(cadr arg-values))]
              [(eq? op 'gt?) (> (car arg-values)(cadr arg-values))]
              [(eq? op 'build) (cons (car arg-values)(cadr arg-values))]
              [(eq? op 'first) (caar arg-values)]
              [(eq? op 'rest) (cdar arg-values)])))

;IF STATEMENTS
(define new-if-exp
  (lambda (cond true false)
    (list 'if cond true false)))
(define if-exp?
  (lambda (exp)
    (if (eq? 'if (car exp)) #t #f)))
(define get-cond
  (lambda (exp)
    (cadr exp)))
(define get-true
  (lambda (exp)
    (caddr exp)))
(define get-false
  (lambda (exp)
    (cadddr exp)))

;LET EXPRESSIONS
(define new-let-exp
  (lambda (symbols expressions body)
    (list 'let symbols expressions body)))
(define let-exp?
  (lambda (exp)
    (if(eq? 'let (car exp)) #t #f)))
(define get-symbols
  (lambda (exp)
    (cadr exp)))
(define get-expressions
  (lambda (exp)
    (caddr exp)))
(define get-body
  (lambda (exp)
    (cadddr exp)))

;LAMBDA EXPRESSIONS
(define new-lambda-exp
  (lambda (params body)
    (list 'lambda params body)))
(define lambda-exp?
  (lambda (exp)
    (eq? (car exp) 'lambda)))


;CLOSURES
(define new-closure
  (lambda (params body env)
    (list 'closure params body env)))
(define closure?
  (lambda (exp)
    (eq? (car exp) 'closure)))
(define get-closure-params
  (lambda (exp)
    (cadr exp)))
(define get-closure-body
  (lambda (exp)
    (caddr exp)))
(define get-closure-env
  (lambda (exp)
    (cadddr exp)))

;ASSIGNMENT EXPRESSIONS
(define new-assign-exp
  (lambda (var val)
    (list 'assign-exp var val)))
(define assign-exp?
  (lambda (exp)
    (eq? (car exp) 'assign-exp)))
(define get-var
  (lambda (exp)
    (cadr exp)))
(define get-val
  (lambda (exp)
    (caddr exp)))

;BEGIN EXPRESSIONS
(define new-begin-exp
  (lambda (expressions)
    (cons 'begin-exp expressions)))
(define begin-exp?
  (lambda (exp)
    (eq? (car exp) 'begin-exp)))
(define begin-expressions
  (lambda(exp)
    (cdr exp)))


