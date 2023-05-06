#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Fall 2022
;; Lab #8
;;
;; Andrew Cox
;; W01026975
;;
;; Purpose of program is to parse an input into an
;; evaluatable format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE GRAMMAR
;;
;; Expression List --> L → _L | EL | ϵ
;; Expression      --> E → DN | AS | (L)
;; Symbol          --> S → AS | ϵ
;; Number          --> N → DN | ϵ
;; Digit           --> D → 0 | 1 | 2 | 3 | ...
;; Symbolic        --> A → a | b | c | d | ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide parse)

;; - NOTE -
;; -------- Last lab8 test initially hung in an infinite loop
;; -------- Debugged with Aran and discovered there was a small bug in eval.rkt
;; -------- Bug was in cond-rec function & has now been fixed
;; -------- All previous labs were passing all tests, even with the bug
;; -------- Confirmed all previous lab tests are still passing with the bug fixed
;; -------- Committed and pushed the bug fix as it does not affect previous lab grading

;; pre-defined parse function from rpn
(define parse
    (lambda (str)
        (first (L (string->list str)))
    )
)

;; pre-defined char-symbolic? check from lab8 pdf
(define char-symbolic?
    (lambda (char)
        (and
            (not (char-whitespace? char))
            (not (eq? char #\())
            (not (eq? char #\)))
        )
    )
)

;; Converts a char containing a digit to the integer value
(define char->number
    (lambda (char)
        (- (char->integer char)
            (char->integer #\0)
        )
    )
)

;; Converts a char containing a symbol to the symbol value
(define char->symbol
    (lambda (char)
        (let*
            (
                (c (string char))
            )
            (string->symbol c)
        )
    )
)

;; Predicate for symbolic values
(define A? char-symbolic?)

;; Predicate for decimal values
(define D? char-numeric?)

;; Predicate for all other values
(define E?
    (lambda (char)
        (or
            (char-numeric? char)
            (char-symbolic? char)
            (equal? #\(  char)
        )
    )
)


;; L → _L | EL | ϵ
(define L
    (lambda (input)
        (cond

            ;; checks for null
            [(null? input) (cons input null)]

            ;; if a space exits, ignore it and continue parsing
            [(char-whitespace? (first input)) (L (rest input))]

            ;; check for expression and pass input to E while parsing the rest
            [(E? (first input))
                (let*
                    (
                        (e-rest (E input))
                        (l-rest (L (rest e-rest)))
                    )

                    (cons
                        (cons (first e-rest) (first l-rest))
                        (rest l-rest)
                    )
                )
            ]

            ;; if char is a close parenthesis,
            ;; use cons to put parenthesis around the previous expression
            [(eq? (first input) #\)) (cons null (rest input))]
        )
    )
)


;; E → DN | AS | (L)
(define E
    (lambda (input)
        (cond

            ;; check for digit and parse digit with N
            [(D? (first input))
                (let*
                    (
                        (d-val (D input))
                        (d-ast (first d-val))
                        (d-rest (rest d-val))
                        (n-rest (N d-rest d-ast))

                    )
                    n-rest
                )
            ]

            ;; check for symbol and parse symbol with S
            [(A? (first input))
                (let*
                    (
                        (a-val (A input))
                        (a-ast (first a-val))
                        (a-rest (rest a-val))
                        (s-rest (S a-rest a-ast))
                    )
                    s-rest
                )
            ]

            ;; if char is open parenthesis
            ;; ignore and continue parsing the rest of the input
            [(eq? (first input) #\( ) (L (rest input))]

            ;; otherwise return an error
            [else (error (string-append "syntax error.  Rest:" (list->string input)))]
        )
    )
)


;; D → 0 | 1 | 2 | 3 | ...
(define D
    (lambda (input)
        (cond

            ;; error if null
            [(null? input) (error (string-append "Not a digit:" (list->string input)))]

            ;; if digit, convert char to number and combine with input
            [(D? (first input))
                (cons
                    (char->number (first input))
                    (rest input)
                )
            ]

            ;; otherwise returns error
            [else (error (string-append "Not a digit:" (list->string input)))]
        )
    )
)


;; N → DN | ϵ
(define N
    (lambda (input num)
        (cond

            ;; when digits are fully parsed, add number back to the input
            [(or (null? input) (not (D? (first input)))) (cons num input)]

            ;; otherwise use carry to calculate digit being represented
            [else
                (let*
                    (
                        (d-rest (D input))
                        (n-rest
                            (N
                                (rest d-rest)
                                (+
                                    (* num 10)
                                    (first d-rest)
                                )
                            )
                        )
                    )
                    n-rest
                )
            ]
        )
    )
)


;; A → a | b | c | d | ...
(define A
    (lambda (input)
        (cond

            ;; base case for null
            [(null? input) (error (string-append "Not an operator:" (list->string input)))]

            ;; check if input is A, convert char and cons back to the input
            [(A? (first input))
                (cons
                    (char->symbol  (first input))
                    (rest input)
                )
            ]

            ;; otherwise return an error
            [else (error (string-append "Not an operator: " (list->string input)))]
        )
    )
)


;; S → AS | ϵ
(define S
    (lambda (input symbol)
        (cond

            ;; checks for null or not a symbol
            [(or (null? input) (not (A? (first input)))) (cons symbol input)]

            ;; otherwise recurses on S to convert and combine chars to make a symbol
            [else
                (let*
                    (
                        (a-rest (A input))
                        (s-rest
                            (S
                                (rest a-rest)
                                (string->symbol (~a symbol (first a-rest))) ;; ~a converts values to strings
                            )
                        )
                    )
                    s-rest
                )
            ]
        )
    )
)
