#lang pl

(: make-bitmask : Integer Integer -> Integer)
;; generates a bitmask for the given field
;; end must be greater than or equal to start
(define (make-bitmask end start)
  (arithmetic-shift (sub1 (arithmetic-shift 1 (add1 (- end start)))) start))

(test (make-bitmask 0 0) => 1)
(test (make-bitmask 3 0) => #b1111)
(test (make-bitmask 9 6) => #b1111000000)

(: make-getter : Integer Integer -> (Integer -> Integer))
;; make a getter for a bitfield
(define (make-getter end start)
  (let ([mask (make-bitmask end start)])
  (lambda (value) (arithmetic-shift (bitwise-and value mask) (- start)))))

(: make-setter : (-> Integer Integer (-> Integer Integer Integer)))
;; make a setter for a bitfield
(define (make-setter end start)
  (let ([mask (make-bitmask end start)])
    (lambda (value field-value)
      (bitwise-ior (bitwise-and value mask) (bitwise-and mask (arithmetic-shift field-value start))))))


(define set-id (make-setter 64 61))

(define-type VM = (List Integer (Vectorof Integer) (Vectorof Integer)))

(define-type Register = Integer)
(define-type Condition = Integer)
(define-type Input = (U Register Integer))
(define-type Opin = Integer)

(define-type Instruction
  [HCF]
  [LDC Register Boolean Integer] ; target upper-half? value
  [LDR Register Register] ; target source
  [STR Regsiter Register]
  [B Register]
  [BC Condition Register Register]
  [ADD Register Input Input]
  [SUB Register Input Input]
  [MUL Register Input Input]
  [DIV Register Input Input]
  [AND Register Input Input]
  [OR Register Input Input]
  [NOT Register Register]
  [IO])
 

(define get-id (make-getter 64 61))

(: decode-instruction (-> Integer Instruction))
(define (decode-instruction instruction)
  (match (get-id instruction)
    [0 (HCF)]
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
        [set-half (make-setter 56 56)]
        [set-value (make-setter 55 24)])
    (set-id 1
            (set-target (LDC-target load)
                        (set-half (if (LDC-upper-half? load) 1 0)
                                  (set-value (LDC-value load) 0))))))

(check-equal? (decode-instruction 0) (HCF))

;(: load LDC)
;(define load (LDC 0 #t 5))
;(check-equal? (decode-instruction (encode-load-constant load)) load)