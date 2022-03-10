#lang racket

(struct Coord (x y) #:transparent)

(define (coord-diff c1 c2)
  (Coord (- (Coord-x c2) (Coord-x c1))
         (- (Coord-y c2) (Coord-y c2))))

;; c2 is inline with c1 if there is some integer n such that
;; c2 = n x c1
;; Assumes c1 ≠ c2
(define (inline? c1 c2)
  (let ([x1 (Coord-x c1)]
        [y1 (Coord-y c1)]
        [x2 (Coord-x c2)]
        [y2 (Coord-y c2)])
    (or
     (and (= y1 0) (= y2 0))
     (and (= x1 0) (= x2 0))
     (and
      (not (= y1 0))
      (not (= x1 0))
      (= (remainder y2 y1) 0)
      (= (remainder x2 x1) 0)
      (= (quotient y2 y1) (quotient x2 x1))))))


(module+ main
  (define *input*
    (map string->list
         (with-input-from-file "inputs/ten-eg1.txt" port->lines)))

  (define *map*
    (flatten
     (for/list ([y (range (length *input*))]
                [row *input*])
       (for/list ([x (range (length row))]
                  [c row]
                  #:when (char=? c #\#))
         (Coord x y)))))
  

  
  )
