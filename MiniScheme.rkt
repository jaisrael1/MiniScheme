;MiniScheme puts together all of the pieces, creating a simplified Scheme environment in which the user can excecute valid code

#lang racket
(require "env.rkt") 
(require "parse.rkt")
(require "interp.rkt")
(require "REP.rkt")

(read-eval-print)
