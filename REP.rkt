;Read-eval-print provides the an input-output loop which allows users to excecute scheme code in MiniScheme
;It handles invalid input, passes valid input to the parser, passes the parse tree to the interpreter, and then prints the output.

#lang racket
(require "parse.rkt")
(require "env.rkt")
(require "interp.rkt")

(provide read-eval-print)

(define read-eval-print
  (lambda ()
    (let ([orig (error-escape-handler)])
      (let/ec exit
        (let retry-loop ()
          (let/ec escape
            (error-escape-handler
             (lambda () (escape #f)))
            (let loop ()
              (begin
                (display "MS> ")
                (let ([in (read)])
                  (if (eq? in 'exit )
                      (begin
                        (printf "returning to Scheme proper~n")
                        (exit #f))
                      (begin
                        (display (eval-exp (parse in) init-env))
                        (newline)
                        (loop)))))))
          (retry-loop)))
      (error-escape-handler orig))))


