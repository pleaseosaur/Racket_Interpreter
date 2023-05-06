#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Fall 2022
;; Lab #7
;;
;; Andrew Cox
;; W01026975
;;
;; Purpose of program is to take a list input, determine
;; the expression, and apply it across all values and
;; any other nested expressions in the list input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide
    evaluate
    lookup
    special-form?
    evaluate-special-form
)


;; pre-defined closure structures from lab7 pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define closure
    (lambda (vars body env)
        (mcons 'closure (mcons env (mcons vars body)))
    )
)

(define closure?
    (lambda (clos)
        (and (mpair? clos) (eq? (mcar clos) 'closure))
    )
)

(define closure-env
    (lambda (clos)
        (mcar (mcdr clos))
    )
)

(define closure-vars
    (lambda (clos)
        (mcar (mcdr (mcdr clos)))
    )
)

(define closure-body
    (lambda (clos)
        (mcdr (mcdr (mcdr clos)))
    )
)

(define set-closure-env!
    (lambda (clos new-env)
        (set-mcar! (mcdr clos) new-env)
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (evaluate input env)
    (cond
        [(number? input) input] ;; if the input is a number, return the input itself
        [(symbol? input) (lookup input env)] ;; if the input is a symbol, lookup the symbol and return the value
        [(special-form? input) (evaluate-special-form input env)] ;; if input is special-form, send to eval
        [(list? input) ;; if input is a list send to apply-function for procedure check
            (apply-function
                (evaluate (car input) env)
                (map
                    (lambda (input)
                        (evaluate input env)
                    )
                    (cdr input)
                )
            )
        ]
        [else (error "unable to evaluate input")]
    )
)


;; helper function to differentiate closures from normal procedures
(define (apply-function listcar listcdr)
    (cond
        [(procedure? listcar) (apply listcar listcdr)] ;; if procedure, apply procedure to list
        [(closure? listcar) (apply-closure listcar listcdr)] ;; if closure, send arguments to apply-closure
        [else (error "unknown function type")]
    )
)


;; creates a local environment and evaluates the closure with that local environment
(define (apply-closure listcar listcdr)
    (evaluate
        (closure-body listcar)
        (append
            (map
                list (closure-vars listcar) listcdr
            )
            (closure-env listcar)
        )
    )
)


;; returns true if the first element is a named special form
;; returns false otherwise
(define (special-form? input)
    (cond
        [(equal? (car input) 'if) #t]
        [(equal? (car input) 'cond) #t]
        [(equal? (car input) 'let) #t]
        [(equal? (car input) 'lambda) #t]
        [(equal? (car input) 'letrec) #t]
        [else #f]
    )
)


;; evaluates named special forms or sends them to respective helper functions
(define (evaluate-special-form input env)
    (cond
        [(equal? (car input) 'if)
            (cond
                ;; for if statments, evaluate the first expression
                [(evaluate (cadr input) env) (evaluate (caddr input) env)] ;; return true consequence
                [else (evaluate (cadddr input) env)] ;; return false consequence
            )
        ]

        [(equal? (car input) 'cond) (cond-rec (cdr input) env)] ;; send cond statements to helper function

        ;; evaluate last expression of let statement and send sublists to helper function to make local environment
        [(equal? (car input) 'let) (evaluate (caddr input) (local-env (cadr input) env))]

        ;; for lambda statements, create a closure
        [(equal? (car input) 'lambda) (closure (cadr input) (caddr input) env)]

        ;; letrec operates similar to let, but needs a call to a different environment helper
        [(equal? (car input) 'letrec) (evaluate (caddr input) (evaluate-letrec (cadr input) env))]

        [else (error "special form not accepted")]
    )
)


;; recursive helper function to evaluate cond statements
(define (cond-rec input env)
    (cond
        [(empty? input) (error "expression not in syntax")] ;; base case to catch conds with no else clause
        [(equal? 'else (car input)) (evaluate (cadr input) env)] ;; else needs to be handled differently
        [(evaluate (caar input) env) (evaluate (cadar input) env)] ;; return true consequence
        [else (cond-rec (cdr input) env)] ;; evaluate next condition if previous condition is false
    )
)


;; helper function for let to create local environment
(define (local-env input env)
    (append
        (map
            (lambda (i)
                ;; evaluate second item of each sublist to return the value
                ;; then put the pairs back together and add them back to input
                (append (append (list(car i)) (list(evaluate (cadr i) env))) input)
            )
            input ;; map over the revised input
        )
        env ;; append updated input to the initial environment to create local environment
    )
)


;; creates a new environment for evaluate-letrec
(define (modify-environment mini-env new-env)
    (map
        (lambda (m)
            (cond
                ;; if the second of the mini environment is a closure, set the closure environment
                [(closure? (cadr m)) (set-closure-env! (cadr m) new-env)]
            )
        )
        mini-env ;; map any set closure environments to the mini environment
    )
)


;; helper function to evaluate letrec with a new environment
(define (evaluate-letrec input env)
    (let*
        (
            ;; map over the input to make the mini-environment
            [mini-env
                (map
                    (lambda (i)
                        (list
                            (car i)
                            (evaluate (cadr i) env)
                        )
                    )
                    input
                )
            ]

            ;; combine the mini and existing environment to make the new-environment
            [new-env (append mini-env env)]
        )
        ;; ensure mini environment is complete then return new environment
        (modify-environment mini-env new-env)
        new-env
    )
)


(define (lookup sym env)
    (cond
        [(not(symbol? sym)) (error "symbol is not a procedure")] ;; check if sym is a symbol
        [(empty? env) (error "input is not in the environment")] ;; if environment list is empty, sym not in list
        [(equal? sym (caar env)) (second (car env))] ;; if they're equal, return the value
        [else (lookup sym (cdr env))] ;; else compare sym to next element in environment
    )
)
