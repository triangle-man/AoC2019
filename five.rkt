#lang racket

;; TODO
;; - Add stdin, stdout, stderr to machine
;; - Decompiler


(module+ main
  ;; Read inputs and initialise machine
  (define inputs
    (with-input-from-file "inputs/five.txt"
      (λ () (string-split (read-line) ","))))

  (define intcode
    (list->vector (map string->number inputs)))

  ;; PART I -- input 1
  ;; PART II -- input 5
  (define program
    (machine-init intcode 0))
  (machine-run! program)

  )

    
(module+ test
  (require rackunit)

  (check-equal? (let ([pgm (machine-init #(1101 100 -1 4 0) 0)])
                  (machine-run! pgm)
                  (peek pgm 4))
                99)
  
  (check-equal? (let ([pgm (machine-init #(1002 4 3 4 33) 0)])
                  (machine-run! pgm)
                  (peek pgm 4))
                99)

  (define pgm1
    (machine-init
     (list->vector (map string->number (string-split
                                        "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                                        ","))) 0))

  )


;; ------------------------------------------------------------
;; Machines

;; A machine is a vector of integers and an instruction pointer
;; Machines are mutable
;; The instruction pointer is #f if the machine has halted
(struct machine (memory ip) #:transparent #:mutable)

;; machine-init
;; Copies initial memory vector
(define (machine-init v ic)
  (machine (vector-copy v) ic))

;; halted? : machine? -> boolean?
(define (halted? M)
  (not (machine-ip M)))

;; halt! : machine? -> void?
;; Stop the machine
(define (halt! M)
  (set-machine-ip! M #f))

;; jump! : machine? addr -> void?
;; Set ip to new address
(define (jump! M addr)
  (set-machine-ip! M addr))

;; skip! : jump relative to instruction pointer
(define (skip! M rel-addr)
  (jump! M (+ rel-addr (machine-ip M))))

;; peek : machine? integer? -> integer?
;; Read and write the memory at position addr
(define (peek M addr)
  (vector-ref (machine-memory M) addr))

;; poke! : machine? integer? integer? -> void?
(define (poke! M addr val)
  (vector-set! (machine-memory M) addr val))

;; Read or write the memory at the position stored in the position addr
(define (mem-read M addr)
  (peek M (peek M addr)))

(define (mem-write! M addr val)
  (poke! M (peek M addr) val))

;; Read and write the memory in the mode given
(define (fetch M addr μ)
  (match μ
    ['position (mem-read M addr)]
    ['immediate (peek M addr)]))

(define (insert! M addr μ val)
  (match μ
    ['position (mem-write! M addr val)]
    ['immediate (poke! M addr val)]))


;; ------------------------------------------------------------
;; Executing machines

;; machine-run : machine? -> void?
;; Run machine until it halts
(define (machine-run! M)
  (let loop ()
    (when (not (halted? M))
      (machine-step! M)
      (loop))))

;; machine-step : machine? -> void?
;; Execute one instruction in a machine
;; pc must not be #f on entry
(define (machine-step! M)
  (let ([ip (machine-ip M)])
    (let-values ([(opcode p1-mode p2-mode p3-mode) (instruction-decode (peek M ip))])
      (match opcode
        ;; All instructions mutate M
        ['halt          (halt! M)]
        ['add           (op-add M p1-mode p2-mode p3-mode)]
        ['mul           (op-mul M p1-mode p2-mode p3-mode)]
        ['readin        (op-readin M p1-mode)]
        ['writeout      (op-writeout M p1-mode)]
        ['jump-if-true  (op-jump-if-true M p1-mode p2-mode)]
        ['jump-if-false (op-jump-if-false M p1-mode p2-mode)]
        ['less-than?    (op<? M p1-mode p2-mode p3-mode)]
        ['equals?       (op=? M p1-mode p2-mode p3-mode)]
        [else           (raise-user-error "Unknown opcode")] 
        ))))

;; Decoding instructions
;; instruction -> (values opcode p1-mode p2-mode p3-mode)
(define (instruction-decode in)
  (let*-values ([(mode  opcode) (quotient/remainder in 100)]
                [(mode~ p1)     (quotient/remainder mode 10)]
                [(p3 p2)        (quotient/remainder mode~ 10)])
    (values (code-sym opcode) (mode-sym p1) (mode-sym p2) (mode-sym p3))))

(define (code-sym op)
  (match op
    [1 'add]
    [2 'mul]
    [3 'readin]
    [4 'writeout]
    [5 'jump-if-true]
    [6 'jump-if-false]
    [7 'less-than?]
    [8 'equals?]
    [99 'halt]
    ))

(define (mode-sym md)
  (match md
    [0 'position]
    [1 'immediate]))

;; ------------------------------------------------------------
;; Opcodes

;; make-op/3 : Make three-parameter opcodes
;; op : (number? number? -> number?)
(define (make-op/3 op)
  (λ (M μ₁ μ₂ μ₃)
    (let ([ip (machine-ip M)])
      (let ([π₁ (fetch M (+ ip 1) μ₁)]
            [π₂ (fetch M (+ ip 2) μ₂)])
        (let ([result (op π₁ π₂)])
          (insert! M (+ ip 3) μ₃ result)))
      (skip! M 4))))

;; make-op/test-and-jump : Make two-parameter opcodes
;; op : (number? -> boolean?)
(define (make-op/test-and-jump op)
  (λ (M μ₁ μ₂)
    (let ([ip (machine-ip M)])
      (let ([π₁   (fetch M (+ ip 1) μ₁)]
            [addr (fetch M (+ ip 2) μ₂)])
        (if (op π₁)
            (jump! M addr)
            (skip! M 3))))))

;; make-op/peek : Make opcodes that send signals
;; writer : number? -> void?
(define (make-op/peek writer)
  (λ (M μ₁)
    (let ([ip (machine-ip M)])
      (writer (fetch M (+ ip 1) μ₁))
      (skip! M 2))))

;; make-op/poke : Opcodes that receive signals
;; reader : -> number?
(define (make-op/poke reader)
  (λ (M μ₁)
    (let ([ip (machine-ip M)])
      (insert! M (+ ip 1) μ₁ (reader))
      (skip! M 2))))

(define op-add
  (make-op/3
   (λ (π₁ π₂)
     (+ π₁ π₂))))

(define op-mul
  (make-op/3
   (λ (π₁ π₂)
     (* π₁ π₂))))

(define op<?
  (make-op/3
   (λ (π₁ π₂)
     (if (< π₁ π₂) 1 0))))

(define op=?
  (make-op/3
   (λ (π₁ π₂)
     (if (= π₁ π₂) 1 0))))

(define op-jump-if-true
  (make-op/test-and-jump
   (compose not zero?)))

(define op-jump-if-false
  (make-op/test-and-jump
   zero?))

(define op-readin
  (make-op/poke
   (λ () (read))))

(define op-writeout
  (make-op/peek
   (λ (v) (println v))))

(define (print-ip ip)
  (printf "[~a] " (~a ip #:min-width 3 #:align 'right)))


