#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RPN Calculator read-eval-print-loop
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "rpn-ast.rkt")

(define repl
  (lambda ()
    (let* ((line (read-line))
           (ast  (parse line)))
      (println ast)
      (repl))))
(repl)
