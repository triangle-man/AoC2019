#lang racket

;; A password is a series of 6 digits,
;; - non-decreasing
;; - with at least one double

;; Equivalently, it is 6 digits;
;; - whose sum is <= 9
;; - with at least one zero

;; Original range: 168630 to 718098 (inclusive?)
;; Valid range to look in: [168888, 777777) 
;; Transformed range: [152000, 700000)

;; Quick-and-dirty version: 
;; Just generate all and then test for sum and zero


(module+ main
  ;; pass[0] is the least-signficant digit
  (define stop (vector 0 0 0 0 0 7))

  ;; Part I
  (define pass1 (vector 0 0 0 2 5 1))
  (let loop ([count 0])
    (if (equal? pass1 stop)
        count
        (begin
          (increment! pass1 0)
          (loop (if (and (small-sum? pass1) (has-zero? pass1))
                    (+ count 1)
                    count)))))

  ;; Part II
  (define pass2 (vector 0 0 0 2 5 1))
  (let loop ([count 0])
    (if (equal? pass2 stop)
        count
        (begin
          (increment! pass2 0)
          (loop (if (and (small-sum? pass2) (has-lone-zero? pass2))
                    (+ count 1)
                    count)))))

  
  )

(define (increment! p position)
  (let ([digit (+ 1 (vector-ref p position))])
    (if (<= digit 9)
        (vector-set! p position digit)
        (begin
          (vector-set! p position 0)
          (increment! p (+ position 1))))))

;; (define (is-valid? p)
;;   (and (small-sum? p) (has-zero? p)))

(define (has-zero? p)
  (or (zero? (vector-ref p 0))
      (zero? (vector-ref p 1))
      (zero? (vector-ref p 2))
      (zero? (vector-ref p 3))
      (zero? (vector-ref p 4))
      (zero? (vector-ref p 5))))

(define (small-sum? p)
  (<= (+ (vector-ref p 0)
         (vector-ref p 1)
         (vector-ref p 2)
         (vector-ref p 3)
         (vector-ref p 4)
         (vector-ref p 5))
      9))

;; Does a zero appear which is not next to another zero?
(define (has-lone-zero? p)
  (let loop [(zeros-so-far 0)
             (rest         (vector->list p))]
    (if (null? rest)
        (eq? zeros-so-far 1) 
        (if (zero? (car rest))
            (loop (+ 1 zeros-so-far) (cdr rest))
            (if (eq? zeros-so-far 1)
                #t
                (loop 0 (cdr rest)))))))
