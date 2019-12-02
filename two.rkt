#lang racket

(module+ main
  ;; Read inputs and initialise machine
  (define inputs
    (with-input-from-file "inputs/two.txt"
      (λ () (string-split (read-line) ","))))

  (define intcode
    (list->vector (map string->number inputs)))

  ;; PART I
  
  (define program
    (machine-init intcode 0))

  (mem-write! program 1 12)
  (mem-write! program 2 2)

  (machine-run! program)
  (printf "Value at position 0 is ~a\n" (mem-read program 0))

  ;; PART II

  (define *required-output* 19690720)
  
  (for* ([noun (in-range 100)]
         [verb (in-range 100)])
    ;; (when (= verb 0) (printf "~a " noun))
    (let ([program (machine-init intcode 0)])
      (mem-write! program 1 noun)
      (mem-write! program 2 verb)
      (machine-run! program)
      (when (= (mem-read program 0) *required-output*)
        (printf "\nSuccess! noun = ~a, verb = ~a\n" noun verb)))))

    
(module+ test
  (define eg1
    (machine
     (list->vector '(1 9 10 3 2 3 11 0 99 30 40 50))
     0))
  (define eg5
    (machine
     (list->vector '(1 1 1 4 99 5 6 0 99))
     0))
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

;; read-mem : machine? integer? -> integer?
;; Read the memory at position addr
(define (mem-read m addr)
  (vector-ref (machine-memory m) addr))

;; write-mem! : machine? integer? integer? -> void?
(define (mem-write! m addr val)
  (vector-set! (machine-memory m) addr val))

;; Read or write the memory at the position stored in the position addr
(define (mem-read-indirect m addr)
  (mem-read m (mem-read m addr)))

(define (mem-write-indirect! m addr val)
  (mem-write! m (mem-read m addr) val))

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
  (let* ([ip     (machine-ip m)]
         [opcode (mem-read m ip)])
    (cond
      [(= opcode 99) (halt! m)]
      [(= opcode 1)  (execute-and-step-four! + m ip)]
      [(= opcode 2)  (execute-and-step-four! * m ip)]
      [else          (halt! m)] ;; Unknown opcode
      )))

;; despatch-four! : procedure? machine? -> void?
(define (execute-and-step-four! op m ip)
  (let ([x (mem-read-indirect m (+ ip 1))]
        [y (mem-read-indirect m (+ ip 2))])
    (let ([result (op x y)])
      (mem-write-indirect! m (+ ip 3) result)))
  (set-machine-ip! m (+ ip 4)))
