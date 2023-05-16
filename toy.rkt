#lang typed/racket

;;; ---<<<TOY>>>----------------------------------------------------
;;; ----------------------------------------------------------------
;;; Syntax

#| The AST:
       <TOY> ::= <num>
               | <id>
               | { bind {{ <id> <TOY> } ... } <TOY> <TOY> ... }
               | { bindrec {{ <id> <TOY> } ... } <TOY> <TOY> ... }
               | { fun { <id> ... } <TOY> <TOY> ... }
               | { rfun { <id> ... } <TOY> <TOY> ... }
               | { if <TOY> <TOY> <TOY> }
               | { <TOY> <TOY> ... }
               | { set! <id> <TOY> }
    |#

;; A matching abstract syntax tree datatype:

(struct Num ([value : Integer]) #:transparent)
(struct Id ([name : Symbol]) #:transparent)
(struct Bind ([names : (Listof Symbol)] [value : (Listof TOY)] [body : (Listof TOY)]) #:transparent)
(struct Bindrec ([names : (Listof Symbol)] [value : (Listof TOY)] [body : (Listof TOY)]) #:transparent)
(struct Fun ([parameters : (Listof Symbol)] [body : (Listof TOY)]) #:transparent)
(struct RFun ([parameters : (Listof Symbol)] [body : (Listof TOY)]) #:transparent)
(struct Call ([function : TOY] [arguments : (Listof TOY)]) #:transparent)
(struct If ([condition : TOY] [then : TOY] [else : TOY]) #:transparent)
(struct Set ([name : Symbol] [value : TOY]) #:transparent)

(define-type TOY (U Num Id Bind Bindrec Fun RFun Call TOY If Set))

(: unique-list? : (Listof Any) -> Boolean)
;; Tests whether a list is unique, guards Bind and Fun values.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

(: parse-sexpr : Sexpr -> TOY)
;; parses s-expressions into TOYs
(define (parse-sexpr sexpr)
  (match sexpr
    [(? number?)    (Num sexpr)]
    [(? symbol?) (Id sexpr)]
    [(cons (and binder (or 'bind 'bindrec)) more) ; binder is now bound to either 'bind or 'bindrec
     (match sexpr
       [(list _ (list (list names nameds) ...) body rest ...)
        #:when (and (list-of? symbol? names
        (if (unique-list? names)
            ((if (symbol=? binder 'bind) Bind Bindrec) 
             names
             (map parse-sexpr nameds)
             (map parse-sexpr (cons body rest)))
            (error 'parse-sexpr "duplicate `~s' names: ~s" binder names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s" binder sexpr)])]
    [(cons (and binder (or 'fun 'rfun)) more)
     (match more
       [(list (list names ...) body rest ...)
        (if (unique-list? names)
            ((if (symbol=? binder 'fun) Fun RFun)
             names
             (map parse-sexpr (cons body rest)))
            (error 'parse-sexpr "duplicate `fun' names: ~s" names))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(cons 'if more)
     (match sexpr
       [(list 'if cond then else)
        (If (parse-sexpr cond)
            (parse-sexpr then)
            (parse-sexpr else))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(cons 'set! rest)
     (match rest
       [(list id value)
        (match id
          [(? symbol?) (Set id (parse-sexpr value))]
          [_ (error 'parse-sexpr "binding must be an identifier" sexpr)])]
       [else (error 'parse-sexpr "bad `set' syntax in ~s" sexpr)])]
    [(list fun args ...) ; other lists are applications
     (Call (parse-sexpr fun)
           (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: list-pred? (All (a) (-> (-> a Boolean) ((Listof a) Boolean))))
(define (list-pred? pred?)
  (λ (loa) (andmap pred? loa)))

(: list-of? (All (a) (-> (-> a Boolean) (Listof a) Boolean)))
(define (list-pred? pred? loa)
  ((list-pred? pred?) loa))

(: parse : String -> TOY)
;; Parses a string containing an TOY expression to a TOY AST.
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;; ----------------------------------------------------------------
;;; Values and environments
(struct EmptyEnv () #:transparent)
(struct FrameEnv ([frame : FRAME] [parent : ENV]) #:transparent)

(define-type ENV (U FrameEnv EmptyENV))

;; a frame is an association list of names and values.
(struct FRAME ([bindings : (Listof Binding)]) #:transparent)

(struct Binding ([name : Symbol] [value : (Boxof VAL)]) #:transparent)

(struct RktV ([val : Any]) #:transparent)
(struct FunV ([reference? : Boolean]
              [parameters : (Listof Symbol)]
              [body : (-> ENV VAL)]
              [closure : ENV]) #:transparent)
(struct PrimV ([val : (-> (Listof VAL) VAL)]) #:transparent)
(struct BogusV () #:transparent)

(define-type VAL (U RktV FunV PrimV BogusV))

(: bogus : VAL)
(define bogus (BogusV))

(: extend : (Listof Symbol) (Listof VAL) ENV -> ENV)
;; extends an environment with a new frame.
(define (extend names values env)
  (raw-extend names (map (inst box VAL) values) env))

(: raw-extend : (Listof Symbol) (Listof (Boxof VAL)) ENV -> ENV)
(define (raw-extend names boxed-values env)
  (if (= (length names) (length boxed-values))
      (FrameEnv (map (lambda ([name : Symbol] [boxed-val : (Boxof VAL)])
                       (list name boxed-val))
                     names boxed-values)
                env)
      (error 'extend "arity mismatch for names: ~s" names)))

(: extend-rec : (Listof Symbol) (Listof (ENV -> VAL)) ENV -> ENV)
;; extend rec
(define (extend-rec names values env)
  (if (= (length names) (length values))
      (let ([frame (extend names (make-list (length names) bogus) env)])
        (for-each (lambda ([name : Symbol] [value : (ENV -> VAL)])
                    (set-box! (lookup name frame) (value frame))) names values)
        frame)
      (error 'extend-rec "arity mismatch for names: ~s" names)))

(: lookup : Symbol ENV -> (Boxof VAL))
;; lookup a symbol in an environment, frame by frame,
;; return its value or throw an error if it isn't bound
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(FrameEnv frame rest)
     (let ([cell (assq name frame)])
       (if cell
           (second cell)
           (lookup name rest)))]))

(: compile-get-boxes : (Listof TOY) -> (ENV -> (Listof (Boxof VAL))))
;; get the boxes bound to the ids
;; I could implement this two ways: traversing elements first or frames first, both are n^2
;; is either one better?
(define (compile-get-boxes exprs)
  (: compile-getter : TOY -> (ENV -> (Boxof VAL)))
  (define (compile-getter expr)
    (cases expr
      [(Id name)
       (lambda ([env : ENV]) (lookup name env))]
      [else
       (lambda ([env : ENV]) (error 'compile-get-boxes "expected symbol, given: ~s" expr))]))
  (unless (unbox compiler?) (error 'compile-get-boxes "Compiler disabled!"))
  (let ([getters (map compile-getter exprs)])
    (lambda (env)
      (map (lambda ([value : (ENV -> (Boxof VAL))]) (value env)) getters))))

(: unwrap-rktv : VAL -> Any)
;; helper for `racket-func->prim-val': unwrap a RktV wrapper in
;; preparation to be sent to the primitive function
(define (unwrap-rktv x)
  (cases x
    [(RktV v) v]
    [else (error 'racket-func "bad input: ~s" x)]))

(: racket-func->prim-val : Function -> (Boxof VAL))
;; converts a racket function to a primitive evaluator function
;; which is a PrimV holding a ((Listof VAL) -> VAL) function.
;; (the resulting function will use the list function as is,
;; and it is the list function's responsibility to throw an error
;; if it's given a bad number of arguments or bad input types.)
(define (racket-func->prim-val racket-func)
  (define list-func (make-untyped-list-function racket-func))
  (box (PrimV (lambda (args)
                (RktV (list-func (map unwrap-rktv args)))))))

;; The global environment has a few primitives:
(: global-environment : ENV)
(define global-environment
  (FrameEnv (list (list '+ (racket-func->prim-val +))
                  (list '- (racket-func->prim-val -))
                  (list '* (racket-func->prim-val *))
                  (list '/ (racket-func->prim-val /))
                  (list '< (racket-func->prim-val <))
                  (list '> (racket-func->prim-val >))
                  (list '= (racket-func->prim-val =))
                  ;; values
                  (list 'true  (box (RktV #t)))
                  (list 'false (box (RktV #f))))
            (EmptyEnv)))

;;; ----------------------------------------------------------------
;;; Evaluation

(: compiler? : (Boxof Boolean))
(define compiler? (box #f))

(: compile : TOY -> (ENV -> VAL))
;; evaluates TOY expressions
(define (compile expr)
  ;; convenient helper
  ;# (: compile* : TOY -> (ENV -> VAL))
  ;# (define (compile* expr) (compile expr env))
  ; convenient helper for running compiled code
  (: caller : ENV -> (ENV -> VAL) -> VAL)
  (define (caller env)
    (lambda (compiled) (compiled env)))
  (unless  (unbox compiler?) (error 'compile "Compiler disabled!"))
  (cases expr
    [(Num n)   (λ ([env : ENV]) (RktV n))]
    [(Id name) (λ ([env : ENV]) (unbox (lookup name env)))]
    [(Bind names exprs bound-body)
     (let ([compiled-exprs (map compile exprs)]
           [compiled-body (compile-list bound-body)])
       (lambda ([env : ENV])
         (compiled-body (extend names (map (caller env) compiled-exprs) env))))]
    [(Bindrec names exprs bound-body)
     (let ([compiled-exprs (map compile exprs)]
           [compiled-body (compile-list bound-body)])
       (lambda ([env : ENV])
         (compiled-body (extend-rec names compiled-exprs env))))]
    [(Fun names bound-body)
     (let ([body (compile-list bound-body)])
       (lambda ([env : ENV])
         (FunV #f names body env)))]
    [(RFun names bound-body)
     (let ([body (compile-list bound-body)])
       (lambda ([env : ENV])
         (FunV #t names body env)))]
    [(Call fun-expr arg-exprs) 
     (let ([compiled-function (compile fun-expr)]
           [compiled-args (map compile arg-exprs)]
           [get-boxes (compile-get-boxes arg-exprs)])
       (lambda ([env : ENV])
         (let ([fval (compiled-function env)]
               [arg-vals (map (caller env) compiled-args)])
           (cases fval
             [(PrimV proc) (proc arg-vals)]
             [(FunV reference? names body fenv)
              (if reference?
                  (body (raw-extend names (get-boxes env) fenv))
                  (body (extend names arg-vals fenv)))]
             [else (error 'eval "function call with non function: ~s" fval)]))))]
    [(If cond-expr then-expr else-expr)
     (let ([cond (compile cond-expr)]
           [then (compile then-expr)]
           [else (compile else-expr)])
       (lambda ([env : ENV])
         (if (cases (cond env)
               [(RktV v) v] ; Racket value => use as boolean
               [else #t]) ; other values are always true
             (then env)
             (else env))))]
    [(Set id value)
     (let ([value (compile value)])
       (lambda ([env : ENV])
         (set-box! (lookup id env) (value env))
         bogus))]))

(: compile-list : (Listof TOY) -> (ENV -> VAL))
;; evaluates a nonempty list of TOY expressions, returning the value of the last one
(define (compile-list exprs)
  (unless (unbox compiler?) (error 'compile-list "Compiler disabled!"))
  (match exprs
    [(list expr) (compile expr)]
    [(cons expr exprs) (let ([compiled (compile expr)] [rest (compile-list exprs)])
                         (λ ([env : ENV])
                           (void (compiled env))
                           (rest env)))]))

(: run : String -> Any)
;; evaluate a TOY program contained in a string
(define (run str)
  (set-box! compiler? #t)
  (let ([compiled (compile (parse str))])
    (set-box! compiler? #f)
    (let ([result (compiled global-environment)])
      (cases (compiled global-environment)
        [(RktV v) v]
        [else (error 'run "evaluation returned a bad value: ~s"
                     result)]))))

;;; ----------------------------------------------------------------
;;; Tests

(test (run "{{fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{bind {{x 3}} x}")
      => 3)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}} {add3 1}}")
      => 4)

(test (run "{bind {{x 3} {y 4}} x}") => 3)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                       {add1 {fun {x} {+ x 1}}}}
                  {add1 {add3 3}}}")
      => 7)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                       {add1 {fun {x} {+ x 1}}}}
                  {bind {{x 3}} {add1 {add3 x}}}}")
      => 7)
(test (run "{bind {{identity {fun {x} x}}
                       {foo {fun {x} {+ x 1}}}}
                  {{identity foo} 123}}")
      => 124)
(test (run "{bind {{x 3}}
                  {bind {{f {fun {y} {+ x y}}}}
                    {bind {{x 5}}
                      {f 4}}}}")
      => 7)
(test (run "{{{fun {x} {x 1}}
                  {fun {x} {fun {y} {+ x y}}}}
                 123}")
      => 124)

;; More tests for complete coverage
(test (run "{bind x 5 x}")      =error> "bad `bind' syntax")
(test (run "{fun x x}")         =error> "bad `fun' syntax")
(test (run "{if x}")            =error> "bad `if' syntax")
(test (run "{}")                =error> "bad syntax")
(test (run "{bind {{x 5} {x 5}} x}") =error> "duplicate*bind*names")
(test (run "{fun {x x} x}")     =error> "duplicate*fun*names")
(test (run "{+ x 1}")           =error> "no binding for")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{1 2}")             =error> "eval: function call with non function: #(struct:RktV 1)")
(test (run "{{fun {x} x}}")     =error> "arity mismatch")
(test (run "{if {< 4 5} 6 7}")  => 6)
(test (run "{if {< 5 4} 6 7}")  => 7)
(test (run "{if + 6 7}")        => 6)
(test (run "{fun {x} x}")       =error> "returned a bad value")


;; exercise 9
(test (run "{bind {{x 5}} { bind {{y {set! x 10}}} x}}") => 10)

;; exercise 12
(test (run "{bindrec {{fact
{fun {n}
{if {= 0 n} 1
{* n {fact {- n 1}}}}}}}
{fact 5}}")
      => 120)

(test (run "{bind {{fact
{fun {n}
{if {= 0 n} 1
{* n {fact {- n 1}}}}}}}
{fact 5}}")
      =error> "lookup: no binding for fact")

(test (run "{bind {{make-counter
{fun {}
{bind {{c 0}}
{fun {}
{set! c {+ 1 c}}
c}}}}}
{bind {{c1 {make-counter}}
{c2 {make-counter}}}
{* {c1} {c1} {c2} {c1}}}}")
      => 6)

(test (run "{bindrec {{foo {fun {}
{set! foo {fun {} 2}}
1}}}
{+ {foo} {* 10 {foo}}}}")
      => 21)

(test (run "{bind {{swap! {rfun {x y}
{bind {{tmp x}}
{set! x y}
{set! y tmp}}}}
{a 1}
{b 2}}
{swap! a b}
{+ a {* 10 b}}}")
      => 12)