#lang typed/racket

(require typed/rackunit)
(require "vm.rkt")

#| The AST:
       <ICARUS> ::= <num|#

(struct Num ([value : Integer]) #:transparent)
(struct PrimOp ([type : BNO-TYPE] [in0 : Icarus] [in1 : Icarus]) #:transparent)
(struct Id ([name : Symbol]) #:transparent)
(struct Binding ([name : (Listof Symbol)] [values : (Listof Icarus)] [body : Icarus]) #:transparent)
(struct Call ([name : Symbol] [args : (Listof Icarus)]) #:transparent)

(define-type Icarus (U Num PrimOp Id Binding Call))

(: parse (-> String Icarus))

(define (parse string)
  (parse-sexpr (string->sexpr string)))

(: parse-sexpr (-> Any Icarus))
(define (parse-sexpr sexpr)
  (match sexpr
    [(? integer?) (Num (exact-round sexpr))]
    [(? symbol?) (Id sexpr)]
    [(list op in0 in1)
     #:when (member op '(+ - * /))
     (PrimOp (symbol->bno-type op) (parse-sexpr in0) (parse-sexpr in1))]
    [(list 'let (list (list name value) ...) body)
     #:when (andmap symbol? name)
     (Binding name (map parse-sexpr value) (parse-sexpr body))]
    [(list name args ...)
     #:when (symbol? name)
     (Call name (map parse-sexpr args))]))

(: symbol->bno-type (-> Symbol BNO-TYPE))
(define (symbol->bno-type symbol)
  (match symbol
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['/ 'div]))

(: string->sexpr (-> String Any))
(define (string->sexpr string)
  (read (open-input-string string)))

(struct Frame ([registers : (Vectorof Boolean)]
               [stack : (Listof (U Symbol #f))]
               [parent : (Optional Frame)]) #:transparent)

(define stack-pointer 7)
(define sp-scratch 6)

(: empty-frame Frame)
(define empty-frame (Frame (make-vector 6 #f) (list #f) none))

(: extend-frame (-> Frame (Listof Symbol) Frame))
(define (extend-frame frame names)
  (Frame (Frame-registers frame)
         (append names (Frame-stack frame))
         (Frame-parent frame)))

(: search-and-reserve! (-> Frame (U Register #f)))
(define (search-and-reserve! frame)
  (let ([register (search-register frame)])
    (when (number? register) (reserve-register! frame register))
    register))
     
(: search-register (-> Frame (U Register #f)))
(define (search-register frame)
  (vector-member #f (Frame-registers frame)))

(: reserve-register! (-> Frame Register Void))
(define (reserve-register! frame register)
  (vector-set! (Frame-registers frame) register #t))

(: free-register! (-> Frame Register Void))
(define (free-register! frame register)
  (vector-set! (Frame-registers frame) register #f))
  
(: lookup (-> Frame Symbol Integer))
(define (lookup frame symbol)
  (let ([idx (index-of (Frame-stack frame) symbol)])
    (if (integer? idx) idx (error "unbound identifier"))))

(: compile (-> Icarus (Listof Instruction)))
(define (compile program)
  (append
   (push stack-pointer)
   (compile-unit program 0 empty-frame)))

(: compile-unit (-> Icarus Integer Frame (Listof Instruction)))
(define (compile-unit expression target frame)
  (: compile-unit* (-> Icarus Integer (Listof Instruction)))
  (define (compile-unit* expression target)
    (compile-unit expression target frame))
  (match expression
    [(Num n) (generate-load target n)]
    [(Id s) (peek-back target (lookup frame s))]
    [(PrimOp type in0 in1)
     (append (compile-unit* in0 target)
             (compile-unit* in1 (add1 target))
             (list (BNO type #f target target (add1 target))))] ;; this is bad
    [(Binding names values body)
     (append
      (save-bindings frame values 0)
      (compile-unit body target (extend-frame frame names)))]))


(: generate-load (-> Integer Integer (Listof Instruction)))
(define (generate-load register n)
  (let ([lower (lower-half n)]
        [upper (upper-half n)])
    (list
      (LDC register #f lower)
      (LDC register #t upper))))

(: push (-> Register (Listof Instruction)))
(define (push register)
  (append (generate-load sp-scratch 1)
          (list (BNO 'add #f stack-pointer stack-pointer sp-scratch)
                (STR stack-pointer register))))

(: pop (-> Register (Listof Instruction)))
(define (pop register)
  (append (generate-load sp-scratch 1)
          (list (BNO 'sub #f stack-pointer stack-pointer 1)
                (LDR register stack-pointer))))

(: peek-back (-> Register Integer (Listof Instruction)))
(define (peek-back register offset)
  (append
   (generate-load sp-scratch offset)
   (list
    (BNO 'sub #f sp-scratch stack-pointer sp-scratch)
    (LDR register sp-scratch))))

(: save-bindings (-> Frame (Listof Icarus) Integer (Listof Instruction)))
(define (save-bindings frame values offset)
  (if (empty? values) '()
      (append
       (save-bindings frame (rest values) (add1 offset))
       (save-binding frame (first values) offset))))

;; warning: doesn't respect register allocation
(: save-binding (-> Frame Icarus Integer (Listof Instruction)))
(define (save-binding frame value offset)
  (append (compile-unit value 0 frame)
          (push 0)))

(: evaluate (-> String Integer))
(define (evaluate string)
  (letrec ([program (compile-unit (parse string) 0 empty-frame)]
           [vm (VM (make-vector 8) (make-vector 1000) void)])
    (load-program! vm program)
    (set-sp! vm 500)
    (run vm 0 reg0 void)))

(: debug (-> String Integer))
(define (debug string)
  (letrec ([program (compile-unit (parse string) 0 empty-frame)]
           [vm (VM (make-vector 8) (make-vector 1000) void)])
    (load-program! vm program)
    (set-sp! vm 500)
    (run vm 0 reg0 print-regs)))

;; test parsing
(check-equal? (parse "17") (Num 17))
(check-equal? (parse "(+ 1 (- 3 2))") (PrimOp 'add (Num 1) (PrimOp 'sub (Num 3) (Num 2))))

;;test expressions
(check-equal? (evaluate "17") 17)
(check-equal? (evaluate "(+ 1 1)") 2)
(check-equal? (evaluate "(+ (- 7 5) (* (/ 6 3) 2))") 6)
(check-equal? (evaluate "(- 36 6)") 30)
(check-equal? (evaluate "(* 5 (- 36 6))") 150)
(check-equal? (evaluate "(+ 5 (/ 36 6))") 11)

;;test let
(check-equal? (evaluate "(let ((x 5)) x)") 5)
(check-equal? (evaluate "(let ([x 5] [y 9]) (* y x))") 45)
(check-equal? (evaluate "(let ((y 6) (z 36)) (+ 5 (/ z y)))") 11)
(check-equal? (evaluate "(let ((x 5) (y 6) (z 36)) (+ x (/ z y)))") 11)

;;test nesting
(check-equal? (evaluate "(let ([x 5]) (let ([y 45]) (/ y x)))") 9)
(check-equal? (evaluate "(let ([x 5]) (let ([y 45]) (let ([z (/ y x)]) (* 2 z))))") 18)