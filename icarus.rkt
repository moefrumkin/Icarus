#lang typed/racket

(require typed/rackunit)
(require "vm.rkt")

#| The AST:
       <ICARUS> ::= <num|#

(struct Num ([value : Integer]) #:transparent)
(struct Id ([name : Symbol]) #:transparent)
(struct Call ([name : Symbol] [args : (Listof Icarus)]) #:transparent)

(define-type Icarus (U Num Id Call))

(: parse (-> String Icarus))

(define (parse string)
  (parse-sexpr (string->sexpr string)))

(: parse-sexpr (-> Any Icarus))
(define (parse-sexpr sexpr)
  (match sexpr
    [(? integer?) (Num (exact-round sexpr))]
    [(? symbol?) (Id sexpr)]
    [(list name args ...)
     #:when (symbol? name)
     (Call name (map parse-sexpr args))]))

(: string->sexpr (-> String Any))
(define (string->sexpr string)
  (read (open-input-string string)))

;; test parsing
(check-equal? (parse "17") (Num 17))
(check-equal? (parse "(+ 1 (- 3 2))") (Call '+ (list (Num 1) (Call '- (list (Num 3) (Num 2))))))

(struct Binding ([name : Symbol] [offset : Natural]) #:transparent)
(struct Frame ([values : (Listof Binding)]) #:transparent)

(: compile (-> Icarus (Listof Instruction)))
(define (compile expression)
  (match expression
    [(Num n) (generate-load 0 n)]))


(: generate-load (-> Integer Integer (Listof Instruction)))
(define (generate-load register n)
  (let ([lower (lower-half n)]
        [upper (upper-half n)])
    (list
      (LDC register #f lower)
      (LDC register #t upper))))

(: reg0 (-> VM Integer))
(define (reg0 vm)
  (vector-ref (VM-registers vm) 0))