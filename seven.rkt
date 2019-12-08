#lang racket

(require "intcode.rkt")

(module+ main
  
  (define amplifier-code
    (with-input-from-file "inputs/seven.txt"
      (thunk (parse-input (read-line)))))

  ;; PART I
  
  (thrust-max amplifier-code)

  ;; PART II

  (max-iter-thrust amplifier-code)
  
  )

(module+ test
  (require rackunit)  

  (define egcode1
    (parse-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))

  (define egcode2
    (parse-input "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))

  (define egcode3
    (parse-input "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))

  (check-equal? (thrust5 egcode1 '(4 3 2 1 0)) 43210)
  (check-equal? (thrust5 egcode2 '(0 1 2 3 4)) 54321)
  (check-equal? (thrust5 egcode3 '(1 0 4 3 2)) 65210 )
  
  (check-equal? (thrust-max egcode1) 43210)
  (check-equal? (thrust-max egcode2) 54321)
  (check-equal? (thrust-max egcode3) 65210 )

  (define egcode4
    (parse-input
     "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))

  (define egcode5
    (parse-input
     "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))

  (check-equal? (thrust-all-passes egcode4 '(9 8 7 6 5))
                139629729)

  (check-equal? (thrust-all-passes egcode5 '(9 7 8 5 6))
                18216)

  (check-equal? (max-iter-thrust egcode4)
                139629729)

  (check-equal? (max-iter-thrust egcode5)
                18216)
  
  )

;; Utility for reading 
(define (parse-input str)
  (list->vector
   (map string->number
        (string-split str ","))))

;; phase is a list of 5 elements
(define (thrust5 ampcode phases)
  (for/fold ([input 0])
            ([phase (in-list phases)])
    (let ([amp (machine-init ampcode 0)])
      (machine-put! amp phase)
      (machine-put! amp input)
      (machine-run! amp)
      (machine-get! amp))))

;; Find maximum thrust 
(define (thrust-max ampcode)
  (let ([all-phases (permutations '(0 1 2 3 4))])
    (apply max
           (map (curry thrust5 ampcode) all-phases))))

;; Functions for Part II

;; Create and set phases of all amplifers
;; -> list-of machine?
(define (initialise-amps intcode phases)
  (map (λ (phase)
         (let ([M (machine-init intcode 0)])
           (machine-put! M phase)
           M))
       phases))

;; Run input through all amplifiers once
;; Amps must be initialised
(define (thrust-one-pass amps in0)
  (for/fold ([in in0])
            ([amp (in-list amps)])
    (machine-put! amp in)
    (machine-run! amp)
    (machine-get! amp)))

;; Run input through all amplifiers until they halt
(define (thrust-all-passes ampcode phases)
  (let ([amps (initialise-amps ampcode phases)])
    (let loop ([in 0])
      (let ([result (thrust-one-pass amps in)])
        (if (ormap halted? amps)
            result
            (loop result))))))

(define (max-iter-thrust ampcode)
   (let ([all-phases (permutations '(5 6 7 8 9))])
    (apply max
           (map (curry thrust-all-passes ampcode) all-phases))) )
