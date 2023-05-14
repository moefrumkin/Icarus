#lang typed/racket

(require typed/rackunit)

(struct None () #:transparent)
(struct (a) Some ([value : a]) #:transparent)
 
(define-type (Optional a) (U None (Some a)))
(define none (None))

(: present? (All (a) (-> (Optional a) Boolean)))
(define (present? optional)
  (match optional
    [(Some a) #t]
    [else #f]))

(: if-present (All (a b) (-> (Optional a) (-> a b) (Optional b))))
(define (if-present optional map)
  (match optional
    [(Some a) (Some (map a))]
    [else none]))

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

(struct VM ([registers : (Vectorof Integer)] [memory : (Vectorof Integer)] [io : (-> VM Void)]))

(: run (All (a) (-> VM Integer (-> VM a) a)))

(define (run vm pc then)
  (define (run*) (run vm (add1 pc) then))
  (define registers (VM-registers vm))
  (define memory (VM-memory vm))
  (match (decode-instruction (vector-ref (VM-memory vm) pc))
    [(HCF) (then vm)]
    [(LDC target upper-half? value) (vector-set! registers
                                                 target
                                                 (if upper-half?
                                                     (arithmetic-shift value 32)
                                                     value))
                                    (run*)]
    [(LDR target source) (vector-set! registers target (vector-ref memory source))
                         (run*)]
    [(STR target source) (vector-set! memory (vector-ref registers target)
                                      (vector-ref memory (vector-ref registers source)))
                         (run*)]
    [(B address link?) (if-present link? (λ ([register : Register])
                                          (vector-set! registers register pc)))
                       (run vm (vector-ref registers address) then)]
    [(BC address condition value) (if (holds? condition (vector-ref registers value))
                                             (run vm (vector-ref registers address) then)
                                             (run*))]
    [(BNO type float? target in0 in1)
     (vector-set! registers target ((bno-type->function type)
                                    (vector-ref registers in0)
                                    (vector-ref registers in1)))
     (run*)]
    [(BBO type target in0 in1)
     (vector-set! registers target ((bbo-type->function type)
                                    (vector-ref registers in0)
                                    (vector-ref registers in1)))
     (run*)]
    [(NOT target source)
     (vector-set! registers target (bitwise-not (vector-ref registers source)))
     (run*)]
    [(IO) ((VM-io vm) vm)
          (run*)]))

(define-type Register Integer)
(define-type Input (U Register Integer))

(struct HCF () #:transparent)
(struct LDC ([target : Register] [upper-half? : Boolean] [value : Integer]) #:transparent)
(struct LDR ([target : Register] [source : Register]) #:transparent)
(struct STR ([target : Register] [source : Register]) #:transparent)
(struct B ([address : Register] [link? : (Optional Register)]) #:transparent)

(define-type Condition (U 'zero 'positive 'negative 'non-zero))

(: condition->num (-> Condition Integer))
(define (condition->num cond)
  (match cond
    ['zero 0]
    ['positive 1]
    ['negative #b10]
    ['non-zero #b11]))

(: num->condition (-> Integer Condition))
(define (num->condition num)
  (match num
    [0 'zero]
    [#b01 'positive]
    [#b10 'negative]
    [#b11 'non-zero]))

(: holds? (-> Condition Integer Boolean))
(define (holds? cond val)
  (match cond
    ['zero (= 0 val)]
    ['positive (> val 0)]
    ['negative (< val 0)]
    ['non-zero (not (= 0 val))]))

(struct BC ([address : Register] [condition : Condition] [value : Register]) #:transparent)

(define-type BNO-TYPE (U 'add 'sub 'mul 'div))

(: bno-type->num (-> BNO-TYPE Integer))
(define (bno-type->num type)
  (match type
    ['add #b1000]
    ['sub #b1001]
    ['mul #b1010]
    ['div #b1011]))

(: bno-type->function (-> BNO-TYPE (-> Integer Integer Integer)))
(define (bno-type->function type)
  (match type
    ['add +]
    ['sub -]
    ['mul *]
    ['div quotient]))

(: num->bno-type (-> Integer BNO-TYPE))
(define (num->bno-type num)
  (match num
    [#b1000 'add]
    [#b1001 'sub]
    [#b1010 'mul]
    [#b1011 'div]))

(struct BNO ([type : BNO-TYPE] [float? : Boolean] [target : Register] [in0 : Register] [in2 : Register]) #:transparent)

(define-type BBO-TYPE (U 'or 'and))

(: bbo-type->num (-> BBO-TYPE Integer))
(define (bbo-type->num type)
  (match type
    ['or #b1100]
    ['and #b1101]))

(: bbo-type->function (-> BBO-TYPE (-> Integer Integer Integer)))
(define (bbo-type->function type)
  (match type
    ['or bitwise-ior]
    ['and bitwise-and]))

(: num->bbo-type (-> Integer BBO-TYPE))
(define (num->bbo-type num)
  (match num
    [#b1100 'or]
    [#b1101 'and]))

(struct BBO ([type : BBO-TYPE] [target : Register] [in0 : Register] [in2 : Register]) #:transparent)

(struct NOT ([target : Register] [source : Register]) #:transparent)

(struct IO () #:transparent)

(define-type Instruction
  (U HCF LDC LDR STR B BC BNO BBO NOT IO))

(define get-id (make-getter 64 61))
(define set-id (make-setter 64 61))

(define-syntax-rule (with-getters ([field end start] ...) body)
  (let ([field (make-getter end start)] ...) body))

(define-syntax-rule (with-setters ([field end start] ...) body)
  (let ([field (make-setter end start)] ...) body))

(define-syntax-rule (decode-bin-bool-op op val)
  (with-getters ([target 60 58]
                 [in0 57 55]
                 [in1 54 52])
    (op (target val) (in0 val) (in1 val))))

(: decode-instruction (-> Integer Instruction))
(define (decode-instruction instruction)
  (define id (get-id instruction))
  (case id
    [(#b0) (HCF)]
    [(#b1) (with-getters ([target 60 58]
                        [register? 57 57]
                        [source 56 54]
                        [upper-half? 56 56]
                        [value 55 24])
           (if (= (register? instruction) 1)
               (LDR (target instruction) (source instruction))
               (LDC (target instruction)
                    (= 1 (upper-half? instruction)) (value instruction))))]
    [(#b10) (with-getters ([target 60 58]
                         [source 57 55])
            (STR (target instruction) (source instruction)))]
    [(#b100) (with-getters ([address 60 58]
                          [link? 57 57]
                          [link-register 56 54])
             (B (address instruction)
                (if (= 1 (link? instruction))
                    (Some (link-register instruction))
                          none)))]
    [(#b111) (with-getters ([address 60 58]
                          [condition 56 55]
                          [value 54 52])
             (BC (address instruction) (num->condition (condition instruction)) (value instruction)))]
    [(#b1000 #b1001 #b1010 #b1011)
     (with-getters ([floating-point? 60 60]
                    [target 59 57]
                    [in0 56 54]
                    [in1 53 51])
       (BNO (num->bno-type id) (= 1 (floating-point? instruction))
            (target instruction) (in0 instruction) (in1 instruction)))]
    [(#b1100 #b1101)
     (with-getters ([target 60 58]
                    [in0 57 55]
                    [in1 54 52])
       (BBO (num->bbo-type id) (target instruction) (in0 instruction) (in1 instruction)))]
    [(#b1110) (with-getters ([target 60 58]
                           [source 57 55])
              (NOT (target instruction) (source instruction)))]
    [(#b1111) (IO)]
    [else (error 'parse-instruction "Instruction Id ~s not recognized" id)]))

(: encode-instruction (-> Instruction Integer))
(define (encode-instruction instruction)
  (match instruction
    [(HCF) 0]
    [(LDR target source)
     (with-setters ([set-target 60 58]
                    [set-register? 57 57]
                    [set-source 56 54])
       (set-id 1
               (set-target target
                           (set-register? 1
                                          (set-source source 0)))))]
    [(LDC target upper-half? value)
     (with-setters ([set-target 60 58]
                    [set-register? 57 57]
                    [set-half 56 56]
                    [set-value 55 24])
       (set-id 1
               (set-target target
                           (set-register? 0
                                          (set-half (if upper-half? 1 0)
                                                    (set-value value 0))))))]
    [(STR target source)
     (with-setters ([set-target 60 58]
                    [set-source 57 55])
       (set-id #b10
               (set-target target
                           (set-source source 0))))]
    [(B address link?)
     (with-setters ([set-address 60 58]
                    [set-link? 57 57]
                    [set-link-register 56 54])
       (set-id #b100
               (set-address address
                            (match link?
                              [(Some register)
                                     (set-link? 1
                                                (set-link-register register 0))]
                              
                              [empty 0]))))]
    [(BC address condition value)
     (with-setters ([set-address 60 58]
                    [set-condition 56 55]
                    [set-value 54 52])
       (set-id #b111
               (set-address address
                            (set-condition (condition->num condition)
                                           (set-value value 0)))))]
    [(BNO type floating-point? target in0 in1)
     (with-setters ([set-floating-point? 60 60]
                    [set-target 59 57]
                    [set-in0 56 54]
                    [set-in1 53 51])
       (set-id (bno-type->num type)
               (set-floating-point? (if floating-point? 1 0)
                                    (set-target target
                                                (set-in0 in0
                                                         (set-in1 in1 0))))))]
    [(BBO type target in0 in1)
     (with-setters ([set-target 60 58]
                    [set-in0 57 55]
                    [set-in1 54 52])
       (set-id (bbo-type->num type)
               (set-target target
                           (set-in0 in0
                                    (set-in1 in1 0)))))]
    [(NOT target source) (with-setters ([set-target 60 58]
                                   [set-source 57 55])
                      (set-id #b1110
                              (set-target target
                                          (set-source source 0)))) ]
    [(IO) (set-id #b1111 0)]))


(check-equal? (let ([set-blue (make-setter 7 0)])
                (set-blue 15  0)) 15)

(check-equal? (let ([set-blue (make-setter 7 0)]
                    [set-green (make-setter 15 8)])
                (set-blue 15 (set-green 9 0))) #b100100001111)

(define-syntax-rule (check-encoding? instruction)
  (check-equal? (decode-instruction (encode-instruction instruction)) instruction))

(check-encoding? (HCF))

(check-encoding? (LDR 4 3))

(check-encoding? (LDC 5 #t 5))
(check-encoding? (LDC 7 #f 392854))

(check-encoding? (STR 4 3))

(check-encoding? (B 2 none))
(check-encoding? (B 2 (Some 7)))

(check-encoding? (BC 5 'negative 3))

(check-encoding? (BNO 'add #f 2 3 4))
(check-encoding? (BNO 'sub #t 4 3 2))
(check-encoding? (BNO 'mul #f 0 6 7))
(check-encoding? (BNO 'div #t 5 3 4))

(check-encoding? (BBO 'or 0 1 2))
(check-encoding? (BBO 'and 5 4 3))

(check-encoding? (NOT 1 7))

(check-encoding? (IO))

(: vec (Vectorof Integer))
(define vec (make-vector 1000))
(vector-set! vec 0 (encode-instruction (LDC 0 #f 1)))
(vector-set! vec 1 (encode-instruction (LDC 1 #f 5)))
(vector-set! vec 2 (encode-instruction (LDC 2 #f 1)))
(vector-set! vec 3 (encode-instruction (LDC 3 #f 4)))
(vector-set! vec 4 (encode-instruction (BNO 'mul #f 0 0 1)))
(vector-set! vec 5 (encode-instruction (BNO 'sub #f 1 1 2)))
(vector-set! vec 6 (encode-instruction (BC 3 'non-zero 1)))
(vector-set! vec 7 (encode-instruction (IO)))

(: io (-> VM Void))
(define (io vm)
  (print (VM-registers vm)))

(define machine (VM (make-vector 8) vec io))

(run machine 0 void)