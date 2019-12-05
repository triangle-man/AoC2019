#lang racket

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
(define (halted? m)
  (not (machine-ip m)))

;; halt! : machine? -> machine?
;; Stop the machine
(define (halt! m)
  (set-machine-ip! m #f))

;; peek : machine? integer? -> integer?
;; Read and write the memory at position addr
(define (peek m addr)
  (vector-ref (machine-memory m) addr))

;; poke! : machine? integer? integer? -> void?
(define (poke! m addr val)
  (vector-set! (machine-memory m) addr val))

;; Read or write the memory at the position stored in the position addr
(define (mem-read m addr)
  (peek m (peek m addr)))

(define (mem-write! m addr val)
  (poke! m (peek m addr) val))

;; Read and write the memory in the mode given
(define (fetch m addr mode)
  (match mode
    ['position (mem-read m addr)]
    ['immediate (peek m addr)]))

(define (insert! m addr mode val)
  (match mode
    ['position (mem-write! m addr val)]
    ['immediate (poke! m addr val)]))


;; ------------------------------------------------------------
;; Executing machines

;; machine-run : machine? -> void?
;; Run machine until it halts
(define (machine-run! m)
  (let loop ()
    (when (not (halted? m))
      (machine-step! m)
      (loop))))

;; machine-step : machine? -> void?
;; Execute one instruction in a machine
;; pc must not be #f on entry
(define (machine-step! m)
  (let ([ip (machine-ip m)])
    (let-values ([(opcode p1-mode p2-mode p3-mode) (instruction-decode (peek m ip))])
      (match opcode
        ;; All instructions mutate m
        ['halt          (halt! m)]
        ['add           (op-execute3 m + p1-mode p2-mode p3-mode)]
        ['mul           (op-execute3 m * p1-mode p2-mode p3-mode)]
        ['readin        (op-input m p1-mode)]
        ['writeout      (op-output m p1-mode)]
        ['jump-if-true  (op-jump-if-true m p1-mode p2-mode)]
        ['jump-if-false (op-jump-if-true m p1-mode p2-mode)]
        ['less-than?    (op-less-than? m p1-mode p2-mode p3-mode)]
        ['equals?       (op-equals? m p1-mode p2-mode p3-mode)]
        [else           (raise-user-error "Unknown opcode")] 
        ))))

;; Decoding instructions
;; instruction -> (values opcode p1-mode p2-mode p3-mode)
(define (instruction-decode in)
  (let*-values ([(modes opcode) (quotient/remainder in 100)]
                [(modes2 p1)    (quotient/remainder modes 10)]
                [(p3 p2)        (quotient/remainder modes2 10)])
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

;; execute3! : Opcodes with two input paramameters and an output parameter
(define (op-execute3 m op p1-mode p2-mode p3-mode)
  (let ([ip (machine-ip m)])
   (let ([x (fetch m (+ ip 1) p1-mode)]
         [y (fetch m (+ ip 2) p2-mode)])
     (let ([result (op x y)])
       (insert! m (+ ip 3) p3-mode result)))
   (set-machine-ip! m (+ ip 4))))

(define (op-input m p1-mode)
  (let ([ip (machine-ip m)])
    (print-ip ip)
    (insert! m (+ ip 1) p1-mode (read))
    (set-machine-ip! m (+ ip 2))))

(define (op-output m p1-mode)
  (let ([ip (machine-ip m)])
    (print-ip ip)
    (printf "~a\n" (fetch m (+ ip 1) p1-mode))
    (set-machine-ip! m (+ ip 2))))

(define (print-ip ip)
  (printf "[~a] " (~a ip #:min-width 3 #:align 'right)))

(define (op-jump-if-true m p1-mode p2-mode)
  (let ([ip (machine-ip m)])
    (if (not (zero? (fetch m (+ ip 1) p1-mode)))
        (set-machine-ip! m (fetch m (+ ip 2) p2-mode))
        (set-machine-ip! m (+ ip 3)))))

(define (op-jump-if-false m p1-mode p2-mode)
  (let ([ip (machine-ip m)])
    (when (zero? (fetch m (+ ip 1) p1-mode))
      (set-machine-ip! m (fetch m (+ ip 2) p2-mode))
      (set-machine-ip! m (+ ip 3)))))

(define (op-less-than? m p1-mode p2-mode p3-mode)
  (let ([ip (machine-ip m)])
    (if (< (fetch m (+ ip 1) p1-mode)
           (fetch m (+ ip 2) p2-mode))
        (insert! m (+ ip 3) p3-mode 1)
        (insert! m (+ ip 3) p3-mode 0))
    (set-machine-ip! m (+ ip 4))))

(define (op-equals? m p1-mode p2-mode p3-mode)
  (let ([ip (machine-ip m)])
    (if (= (fetch m (+ ip 1) p1-mode)
           (fetch m (+ ip 2) p2-mode))
        (insert! m (+ ip 3) p3-mode 1)
        (insert! m (+ ip 3) p3-mode 0))
    (set-machine-ip! m (+ ip 4))))
