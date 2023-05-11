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
(struct STR ([source : Register] [target : Register]) #:transparent)
(struct B ([addr : Register]))
(struct RET ())
(struct BC ([cond : Condition] [value : Register] [addr : Register]))

(define-syntax-rule (binary-num-op op)
  (struct op ([integer? : Boolean] [in0 : Input] [in2 : Input])))

(define-syntax-rule (binary-num-ops op ...)
  (begin (binary-num-op op) ...))

(binary-num-ops ADD SUB MUL DIV)

(define-syntax-rule (binary-bool-op op)
  (struct op ([in0 : Opin] [in2 : Opin])))

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
    [0 (HCF)]
    [1 (decode-load instruction)]
    [id (error 'parse-instruction "Instruction Id ~s not recognized" id)]))

(: decode-load (-> Integer (U LDR LDC)))
(define (decode-load instruction)
  (let ([target (make-getter 60 58)]
        [register? (make-getter 57 57)]
        [source (make-getter 56 54)]
        [upper-half? (make-getter 55 55)]
        [value (make-getter 55 24)])
    (if (register? instruction)
        (LDR (target instruction) (source instruction))
        (LDC (target instruction) (if (= 0 (upper-half? instruction)) #f #t) (value instruction)))))

(: encode-load-constant (-> LDC Integer))
(define (encode-load-constant load)
  (let ([set-target (make-setter 60 58)]
        [set-register? (make-setter 57 57)]
        [set-half (make-setter 56 56)]
        [set-value (make-setter 55 24)])
    (set-id 1
            (set-target (LDC-target load)
                        (set-register? 0
                                      (set-half (if (LDC-upper-half? load) 1 0)
                                                (set-value (LDC-value load) 0)))))))

(check-equal? (decode-instruction 0) (HCF))

(define load (LDC 5 #t 5))
(check-equal? (encode-load-constant load) 0)