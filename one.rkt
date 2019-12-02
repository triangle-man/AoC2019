#lang racket

;; ------------------------------------------------------------
;; inputs : Listof string?
(define inputs
  (with-input-from-file "inputs/one.txt" port->lines))

(define masses (map string->number inputs))

;; ------------------------------------------------------------
;; Part 1: total fuel

(define (fuel mass)
  (- (floor (/ mass 3)) 2))

(apply + (map fuel masses))

;; ------------------------------------------------------------
;; Part 2: with fuel for fuel

;; all-fuels : mass -> mass
(define (all-fuels mass)
  (if (<= mass 0)
      0
      (+ mass (all-fuels (fuel mass)))))

;; Fuel mass is total mass less mass of modules
(-
 (apply + (map all-fuels masses))
 (apply + masses))
