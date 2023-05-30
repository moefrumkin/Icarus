#lang typed/racket

(require typed/rackunit)
(require "vm.rkt")

#| The AST:
       <ICARUS> ::= <num|#

(struct Num ([value : Integer]) #:transparent)
(struct PrimOp ([type : BNO-TYPE] [head : Icarus] [tail : (Listof Icarus)]) #:transparent)
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
    [(list op head tail ...)
     #:when (member op '(+ - * /))
     (PrimOp (symbol->bno-type op)
             (parse-sexpr head)
             (map parse-sexpr tail))]
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

;; is the register reserved by the compiler for a specific role?
(: forbidden? (-> Register Boolean))
(define (forbidden? register)
  (or (= register stack-pointer) (= register sp-scratch)))

(: make-empty-frame (-> Frame))
(define (make-empty-frame) (Frame (vector #t #f #f #f #f #f #t #t) (list #f) none))

(: extend-frame (-> Frame (Listof Symbol) Frame))
(define (extend-frame frame names)
  (Frame (Frame-registers frame)
         (append names (Frame-stack frame))
         (Frame-parent frame)))

;; searches for an empty register and reserve it if one exists. Otherwise returns false.
(: search-and-reserve! (-> Frame (U Register #f)))
(define (search-and-reserve! frame)
  (let ([register (search-register frame)])
    (when (number? register) (reserve-register! frame register))
    register))
     
(: search-register (-> Frame (U Register #f)))
(define (search-register frame)
  (: search-register (-> Integer (Vectorof Boolean) (U Register #f)))
  (define (search-register idx registers)
    (if (= idx (vector-length registers)) #f
        (if (vector-ref registers idx)
            (search-register (add1 idx) registers)
            idx)))
  (search-register 0 (Frame-registers frame)))

(: reserve-register! (-> Frame Register Void))
(define (reserve-register! frame register)
  (vector-set! (Frame-registers frame) register #t))

(: free-register! (-> Frame Register Void))
(define (free-register! frame register)
  (vector-set! (Frame-registers frame) register #f))

(: reserve-next-free! (-> Frame Register Register))
(define (reserve-next-free! frame start)
  (let ([register (next-free-register start)])
    (reserve-register! frame register)
    register))

(: next-free-register (-> Register Register))
(define (next-free-register current)
  (let ([next (modulo (add1 current) 7)])
    (if (forbidden? next)
        (next-free-register next)
        next)))

;;wraps a compil lambda witha  procedure guaranteeting a free register
(: wrap-reserve (-> Frame (-> Frame Register (Listof Instruction)) (Listof Instruction)))
(define (wrap-reserve frame compile)
  (let ([register (search-and-reserve! frame)])
    (if register
        (compile frame register)
        (letrec ([register (reserve-next-free! frame 0)]
                 [program (append
                           (push register)
                           (compile frame register)
                           (pop register))])
          (free-register! frame register)
          program))))
  
(: lookup (-> Frame Symbol Integer))
(define (lookup frame symbol)
  (let ([idx (index-of (Frame-stack frame) symbol)])
    (if (integer? idx) idx (error "unbound identifier"))))

(: compile (-> String (Listof Instruction)))
(define (compile program)
  (compile-unit (parse program) 0 (make-empty-frame)))

(: compile-unit (-> Icarus Integer Frame (Listof Instruction)))
(define (compile-unit expression target frame)
  (: compile-unit* (-> Icarus Integer (Listof Instruction)))
  (define (compile-unit* expression target)
    (compile-unit expression target frame))
  (match expression
    [(Num n) (generate-load target n)]
    [(Id s) (peek-back target (lookup frame s))]
    [(PrimOp type head tail)
     (wrap-reserve frame
                   (lambda (frame scratch)
                     (append (compile-unit head target frame)
                             (foldl (lambda ([icarus : Icarus] [prog : (Listof Instruction)])
                                      (append
                                       prog
                                       (compile-unit icarus scratch frame)
                                       (list (BNO type #f target target scratch))))
                                    '()
                                    tail))))]
    [(Binding names values body)
     (append
      (save-bindings frame values)
      (compile-unit body target (extend-frame frame names)))]))

;;loads a constant into a register
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
          (list (LDR register stack-pointer)
                (BNO 'sub #f stack-pointer stack-pointer sp-scratch))))

(: peek-back (-> Register Integer (Listof Instruction)))
(define (peek-back register offset)
  (append
   (generate-load sp-scratch offset)
   (list
    (BNO 'sub #f sp-scratch stack-pointer sp-scratch)
    (LDR register sp-scratch))))

(: save-bindings (-> Frame (Listof Icarus) (Listof Instruction)))
(define (save-bindings frame values)
  (if (empty? values) '()
      (append
       (save-bindings frame (rest values))
       (save-binding frame (first values)))))

;; pushes a binding's value to the stack
(: save-binding (-> Frame Icarus (Listof Instruction)))
(define (save-binding frame value)
  (wrap-reserve frame (lambda (frame register)
                  (append (compile-unit value register frame)
                          (push register)))))

(: evaluate (-> String Integer))
(define (evaluate string)
  (letrec ([program (compile string)]
           [vm (VM (make-vector 8) (make-vector 1000) void)])
    (load-program! vm program)
    (set-sp! vm 500)
    (run vm 0 reg0 void)))

(: debug (-> String Integer))
(define (debug string)
  (letrec ([program (compile-unit (parse string) 0 (make-empty-frame))]
           [vm (VM (make-vector 8) (make-vector 1000) void)])
    (load-program! vm program)
    (set-sp! vm 500)
    (run vm 0 reg0 print-regs)))

;; test parsing
(check-equal? (parse "17") (Num 17))
(check-equal? (parse "(+ 1 (- 3 2))")
              (PrimOp 'add (Num 1) (list (PrimOp 'sub (Num 3) (list (Num 2))))))

;;test expressions
(check-equal? (evaluate "17") 17)
(check-equal? (evaluate "(+ 1 1)") 2)
(check-equal? (evaluate "(+ (- 7 5) (* (/ 6 3) 2))") 6)
(check-equal? (evaluate "(- 36 6)") 30)
(check-equal? (evaluate "(* 5 (- 36 6))") 150)
(check-equal? (evaluate "(+ 5 (/ 36 6))") 11)

;;test n-ary operations
(check-equal? (evaluate "(+ 1 2 3 4 5 6 7)") 28)
(check-equal? (evaluate "(+ 10 20 30 40 50 60 70 80 90 100)") 550)
(check-equal? (evaluate "(/ 144 3 4)") 12)
(check-equal? (evaluate "(* 1 2 3 4 5 6)") 720)

;;test let
(check-equal? (evaluate "(let ((x 5)) x)") 5)
(check-equal? (evaluate "(let ([x 5] [y 9]) (* y x))") 45)
(check-equal? (evaluate "(let ((y 6) (z 36)) (+ 5 (/ z y)))") 11)
(check-equal? (evaluate "(let ((x 5) (y 6) (z 36)) (+ x (/ z y)))") 11)

;;test nesting
(check-equal? (evaluate "(let ([x 5]) (let ([y 45]) (/ y x)))") 9)
(check-equal? (evaluate "(let ([x 5]) (let ([y 45]) (let ([z (/ y x)]) (* 2 z))))") 18)

;;test register allocation
(check-equal? (evaluate
              "(let ([a 1] [b 2] [c 3] [d 4] [e 5] [f 6] [g 8] [h 9])
                (+ a b c d e f g h))") 45)