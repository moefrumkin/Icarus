#lang typed/racket

#| The AST:
       <TOY> ::= <num>
               | <id>
               | { bind {{ <id> <TOY> } ... } <TOY> <TOY> ... }
               | { fun { <id> ... } <TOY> <TOY> ... }
               | { <TOY> <TOY> ... }
|#

(struct Num ([value : Integer]) #:transparent)
(struct Id ([name : Symbol]) #:transparent)
(struct Call ([name : Symbol] [args : (Listof Icarus)] [body : Icarus]) #:transparent)

(define-type Icarus (U Num Id Call))

(: parse-sexpr (-> S-exp Icarus))
(define (parse-sexpr sexpr)
  (match sexpr
    [(? integer?) (Num sexpr)]
    [(? symbol?) (Id sexpr)]
    [(list name args ... body)
     #:when (symbol? name)
     (Call name (map parse-sexpr args) (parse-sexpr body))]))



(: list-pred? (All (a) (-> (-> a Boolean) (-> (Listof a) Boolean))))
(define (list-pred? pred?)
  (λ (loa) (andmap pred? loa)))

(: list-of? (All (a) (-> (-> a Boolean) (Listof a) Boolean)))
(define (list-of? pred? loa)
  ((list-pred? pred?) loa))