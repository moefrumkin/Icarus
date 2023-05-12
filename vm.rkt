#lang typed/racket

(require typed/rackunit)

(: make-bitmask : (-> Integer Integer Integer))
;; generates a bitmask for the given field
;; end must be greater than or equal to start
(define (make-bitmask end start)
  (arithmetic-shift (sub1 (arithmetic-shift 1 (add1 (- end start)))) start))

(check-equal? (make-bitmask 0 0) 1)
(check-equal? (make-bitmask 3 0) #b1111)
(check-equal? (make-bitmask 9 6) #b1111000000)

(: make-getter : (-> Integer Integer (-> Integer Integer)))
;; make a getter for a bitfield
(define (make-getter end start)
  (let ([mask (make-bitmask end start)])
  (lambda (value) (arithmetic-shift (bitwise-and value mask) (- start)))))

(: make-setter : (-> Integer Integer (-> Integer Integer Integer)))
;; make a setter for a bitfield
(define (make-setter end start)
  (let ([mask (make-bitmask end start)])
    (lambda (field-value value)
      (bitwise-ior
       (bitwise-and (bitwise-not mask) value)
       (bitwise-and mask (arithmetic-shift field-value start))))))

(check-equal? (let ([set-blue (make-setter 7 0)])
                (set-blue 15  0)) 15)

(check-equal? (let ([set-blue (make-setter 7 0)]
                    [set-green (make-setter 15 8)])
                (set-blue 15 (set-green 9 0))) #b100100001111)

(struct VM ([pc : Integer] [registers : (Vectorof Integer)] [memory : (Vectorof Integer)]))

(define-type Register Integer)
(define-type Condition Integer)
(define-type Input (U Register Integer))
(define-type Opin Integer)

(struct HCF () #:transparent)
(struct LDC ([target : Register] [upper-half? : Boolean] [value : Integer]) #:transparent)
(struct LDR ([target : Register] [source : Register]) #:transparent)
(struct STR ([target : Register] [source : Register]) #:transparent)
(struct B ([address : Register] [link? : Boolean]) #:transparent)
(struct RET () #:transparent)
(struct BC ([address : Register] [condition : Condition] [value : Register]) #:transparent)

(define-syntax-rule (binary-num-op op)
  (struct op ([integer? : Boolean] [in0 : Input] [in2 : Input]) #:transparent))

(define-syntax-rule (binary-num-ops op ...)
  (begin (binary-num-op op) ...))

(binary-num-ops ADD SUB MUL DIV)

(define-syntax-rule (binary-bool-op op)
  (struct op ([in0 : Opin] [in2 : Opin]) #:transparent))

(define-syntax-rule (binary-bool-ops op ...)
  (begin (binary-bool-op op) ...))

(binary-bool-ops AND OR)

(struct NOT ([target : Register] [source : Register]))

(struct IO ())

(define-type Instruction
  (U HCF LDC LDR STR B RET BC ADD SUB MUL DIV AND OR NOT IO))

(define get-id (make-getter 64 61))
(define set-id (make-setter 64 61))

(: decode-instruction (-> Integer Instruction))
(define (decode-instruction instruction)
  (match (get-id instruction)
    [#b0 (HCF)]
    [#b1 (decode-load instruction)]
    [#b10 (decode-store instruction)]
    [#b100 (decode-branch instruction)]
    [#b101 (RET)]
    [#b111 (decode-conditional-branch instruction)]
    [id (error 'parse-instruction "Instruction Id ~s not recognized" id)]))

(: encode-instruction (-> Instruction Integer))
(define (encode-instruction instruction)
  (match instruction
    [(HCF) 0]
    [(struct LDR _) (encode-load-register instruction)]
    [(struct LDC _) (encode-load-constant instruction)]
    [(struct STR _) (encode-store instruction)]
    [(struct B _) (encode-branch instruction)]
    [(struct RET _) (set-id #b101 0)]
    [(struct BC _) (encode-conditional-branch instruction)]))

(define-syntax with-getters
  (syntax-rules (< : > = )
    [(with-getters ([field end start] ...) body)
     (let ([field (make-getter end start)] ...) body)]))

(define-syntax with-setters
  (syntax-rules (< : > = )
    [(with-getters ([field end start] ...) body)
     (let ([field (make-setter end start)] ...) body)]))

(: decode-load (-> Integer (U LDR LDC)))
(define (decode-load instruction)
  (with-getters ([target 60 58]
                 [register? 57 57]
                 [source 56 54]
                 [upper-half? 56 56]
                 [value 55 24])
    (if (= (register? instruction) 1)
        (LDR (target instruction) (source instruction))
        (LDC (target instruction) (if (= 1 (upper-half? instruction)) #t #f) (value instruction)))))

(: decode-store (-> Integer STR))
(define (decode-store instruction)
  (with-getters ([target 60 58]
                 [source 57 55])
    (STR (target instruction) (source instruction))))

(: decode-branch (-> Integer B))
(define (decode-branch instruction)
  (with-getters ([address 60 58]
                 [link? 57 57])
    (B (address instruction) (if (= 1 (link? instruction)) #t #f))))

(: decode-conditional-branch (-> Integer BC))
(define (decode-conditional-branch instruction)
  (with-getters ([address 60 58]
                 [condition 56 55]
                 [value 54 52])
    (BC (address instruction) (condition instruction) (value instruction))))

(: encode-load-constant (-> LDC Integer))
(define (encode-load-constant load)
  (with-setters ([set-target 60 58]
                 [set-register? 57 57]
                 [set-half 56 56]
                 [set-value 55 24])
    (set-id 1
            (set-target (LDC-target load)
                        (set-register? 0
                                      (set-half (if (LDC-upper-half? load) 1 0)
                                                (set-value (LDC-value load) 0)))))))

(: encode-load-register (-> LDR Integer))
(define (encode-load-register load)
  (with-setters ([set-target 60 58]
                 [set-register? 57 57]
                 [set-source 56 54])
    (set-id 1
            (set-target (LDR-target load)
                        (set-register? 1
                                       (set-source (LDR-source load) 0))))))

(: encode-store (-> STR Integer))
(define (encode-store store)
  (with-setters ([set-target 60 58]
                 [set-source 57 55])
    (set-id #b10
         (set-target (STR-target store)
                     (set-source (STR-source store) 0)))))

(: encode-branch (-> B Integer))
(define (encode-branch branch)
  (with-setters ([set-address 60 58]
                 [set-link? 57 57])
    (set-id #b100
            (set-address (B-address branch)
                         (set-link? (if (B-link? branch) 1 0) 0)))))

(: encode-conditional-branch (-> BC Integer))
(define (encode-conditional-branch conditional-branch)
  (with-setters ([set-address 60 58]
                 [set-condition 56 55]
                 [set-value 54 52])
    (set-id #b111
            (set-address (BC-address conditional-branch)
                         (set-condition (BC-condition conditional-branch)
                                          (set-value (BC-value conditional-branch) 0))))))



(: enc-dec (-> Instruction Instruction))
(define (enc-dec instruction)
  (decode-instruction (encode-instruction instruction)))

(check-equal? (decode-instruction 0) (HCF))

(check-equal? (decode-instruction (encode-instruction (LDR 4 3)))(LDR 4 3))

(check-equal? (decode-instruction (encode-instruction (LDC 5 #t 5))) (LDC 5 #t 5))
(check-equal? (decode-instruction (encode-instruction (LDC 7 #f 392854))) (LDC 7 #f 392854))

(check-equal? (decode-instruction (encode-instruction (STR 4 3)))(STR 4 3))

(check-equal? (decode-instruction (encode-instruction (B 2 #t))) (B 2 #t))

(check-equal? (enc-dec (RET)) (RET))

(check-equal? (enc-dec (BC 5 #b10 3)) (BC 5 #b10 3))