#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RPN Calculator parsing
;; Winter 2021
;;
;; To use this parser, start an interactive racket session and require the
;; module.  The call parse with a string.  For example:
;;
;;  $ racket
;;  > (require "rpn-ll.rkt")
;;  > (parse "123 456 +")
;;  '(123 456 '+)
;;
;; Each of the functions for the grammar's variables will take the list of input
;; symbols (characters) and return a pair consisting of a variables parsed value
;; and the remaining input.  For example,
;;
;; > (P '(#\+ #\space))
;; '('+ #\space)
;;
;; Notice that the plus character is a symbol in the return.
;;
;;
;; Grammar:         NULL  First      Follow
;; S->_S|PS|DNT|e   yes   _+-*/0..9  $
;; T->_S|PS|e       yes   _+-*/      $
;; N->DN|e          yes   0..0       _+-*/$
;; D->0..9          no    0..9       _+-*/0..9$
;; P->+|-|*|/       no    +-*/       _+-*/0..9$
;;
;; Prediction:
;;     P    _   D   null
;; S-> PS   _S  DNT e
;; T-> PS   _S  err e
;; N-> e    e   DN  e
;; D-> err err  D   err
;; P-> P   err  err err
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse)

;; This is a debugging tool from racket.
;; After each function declaration is a commented-out trace for that function.
;; If you want to see each call to the functions un-comment the trace
;; declaration, reload, and parse something. 
(require racket/trace)

;; The main parser
;; 
(define parse
  (lambda (str)
    (first (S (string->list str)))
    )
  )

;; S
;; prediction
;;     P    _   D   null
;; S-> PS   _S  DNT e
(define S
  (lambda (input)
    (cond
     [(null? input) (cons null null)]
     
     [(char-whitespace? (first input)) (S (rest input))]
     
     [(P? (first input)) (let* ((p-rest (P input))
                                (s-rest (S (rest p-rest))))
                           (cons (cons (first p-rest) (rest s-rest))
                                 (rest s-rest)))]
     
     [(D? (first input)) (let* ((d-rest (D input))
                                (n-rest (N (rest d-rest)))
                                (t-rest (T (rest n-rest))))
                           (cons (cons (cons (first d-rest)
                                             (first n-rest))
                                       (first t-rest))
                                 (rest t-rest)))]
     )
    )
  )
;;(trace S)

;; T
;; prediction
;;     P    _   D   null
;; T-> PS   _S  err e
(define T
  (lambda (input)
    (cond
     [(null? input) (cons '() null)]

     [(char-whitespace? (first input)) (S (rest input))]

     [(P? (first input)) (let* ((p-rest (P input))
                                (s-rest (S (rest p-rest))))
                           
                           (cons (first p-rest (first s-rest))
                                 (rest s-rest)))]
     
     [else (error (string-append "syntax error.  Rest:"
                                 (list->string input)))]
     )
    )
  )
;;(trace T)

;; N
;; prediction
;;     P    _   D   null
;; N-> e    e   DN  e
(define N
  (lambda (input)
    (cond
     [(null? input) (cons '() null)]

     [(D? (first input)) (let* ((d-rest (D input))
                                (n-rest (N (rest d-rest))))
                           (cons (cons (first d-rest) (first n-rest))
                                 (rest n-rest)))]

     [else (cons '() input)]
     )
    )
  )
;;(trace N)


;; D
;; This is not really a rule per se.  In a typical parser, this, like P (below),
;; would use a regular-expresion tokenizer to find the terminal symbols of the
;; language.  In this case, the symbol would be D, but it would also carry the
;; value of the integer.  For example, after reading "123" it would return
;; D(123).  The parser just works with D. 
;;
;; prediction
;;     P    _   D   null
;; D-> err err  D   e
(define D
  (lambda (input)
    (cond
     [(D? (first input)) (cons (char->number (first input))
                               (rest input))]
           
      [else (error (string-append "Not a digit:"
                                  (list->string input)))]
      )
    )
  )
;;(trace D)

;; P
;; See the note with D (above). 
;; prediction
;;     P    _   D   null
;; P-> P   err  err err
(define P 
  (lambda (input)
    (cond
     
     [(P? (first input)) (cons (string->symbol (string (first input)))
                               (rest input))]
     
     [else (error (string-append "Not an operator: "
                                 (list->string input)))]
     )
    )
  )
;;(trace P)


;; Char to Number
;; Converts a char containing a digit to the integer value.  For example
;; (char->number #\4) => 4.
(define char->number
  (lambda (char)
    (- (char->integer char)
       (char->integer #\0))
    )
  )

;; Predicate for decimal values.
(define D? char-numeric?)

;; Predicate for operations or procedures. 
(define P?
  (lambda (char)    
    (or (eq? #\+ char)
        (eq? #\- char)
        (eq? #\* char)
        (eq? #\/ char))
    )
  )
  
