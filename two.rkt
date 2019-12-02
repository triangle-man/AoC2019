#lang racket

(module+ main
  ;; Read inputs and initialise machine
  (define inputs
    (with-input-from-file "inputs/two.txt"
      (λ () (string-split (read-line) ","))))

  (define intcode
    (list->vector (map string->number inputs)))

  (define program
    (machine intcode 0))

  ;; PART I
  
  ;; Fix machine
  (mem-write! program 1 12)
  (mem-write! program 2 2)

  ;; Run 
  (machine-run! program)
  (printf "Value at position 0 is ~a\n" (mem-read program 0)))




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

;; A machine is a vector of integers and a program counter
;; Machines are mutable
;; The programme counter is #f if the machine has halted
(struct machine (memory pc) #:transparent #:mutable)

;; halted? : machine? -> boolean?
(define (halted? m)
  (not (machine-pc m)))

;; halt! : machine? -> machine?
;; Stop the machine
(define (halt! m)
  (set-machine-pc! m #f))

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
  (let* ([pc     (machine-pc m)]
         [opcode (mem-read m pc)])
    (cond
      [(= opcode 99) (halt! m)]
      [(= opcode 1)  (execute-and-step-four! + m pc)]
      [(= opcode 2)  (execute-and-step-four! * m pc)]
      [else
       (raise-user-error "Unknown opcode ~a at pc = ~a" opcode pc)]))
  )

;; despatch-four! : procedure? machine? -> void?
(define (execute-and-step-four! op m pc)
  (let ([x (mem-read-indirect m (+ pc 1))]
        [y (mem-read-indirect m (+ pc 2))])
    (let ([result (op x y)])
      (mem-write-indirect! m (+ pc 3) result)))
  (set-machine-pc! m (+ pc 4)))
