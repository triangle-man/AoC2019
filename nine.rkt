#lang racket

(require "intcode.rkt")

(module+ main

  (define inputs
    (with-input-from-file "inputs/nine.txt"
      (λ () (string-split (read-line) ","))))

  (define boost-code
    (list->vector (map string->number inputs)))

  ;; PART I
  
  (define boost
    (machine-init boost-code 0))

  (machine-put! boost 1)
  (machine-run! boost)
  (machine-get! boost)

  (define boost2
    (machine-init boost-code))

  (machine-put! boost2 2)
  (machine-run! boost2)
  (machine-get! boost2)
  
  )
