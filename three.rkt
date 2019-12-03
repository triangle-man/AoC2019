#lang racket

(module+ main

  ;; Read and parse input wires
  (define-values (in1 in2)
    (with-input-from-file "inputs/three.txt"
      (λ () (values (string-split (read-line) ",")
                    (string-split (read-line) ",")))))

  
  #f)

;; Positions
;; ---------

;; A posn is two integer coordinates:
(struct posn (x y) #:transparent)

;; Two positions are ordered if either their x-values are the same and the
;; y-values are ordered; or their y-values are the same and their x-values are ordered
(define (posn<=? p1 p2)
  (cond
    [(= (posn-x p1) (posn-x p2)) (<= (posn-y p1) (posn-y p2))]
    [(= (posn-y p1) (posn-y p2)) (<= (posn-x p1) (posn-x p2))]
    [else #f]))

;; Segments
;; --------

;; A segment is a pair of posn's.
(struct segment (p1 p2) #:transparent) ;; such that (posn<=? p1 p2) is #t

(define (horizontal? seg)
  (= (posn-y (segment-p1 seg)) (posn-y (segment-p2 seg))))

;; A segmented is ordered if its two points are ordered
(define (ordered? seg)
  (posn<=? (segment-p1 seg) (segment-p2 seg)))

;; Make-ordered assumes seg is horizontal or vertical
(define (make-ordered seg)
  (if (ordered? seg)
      seg
      (segment (segment-p2 seg) (segment-p1 seg))))

;; The intersection of two ordered segments is
;; - a posn, or
;; - #f if they do not intersect, or
;; - #f if they are both horizontal or both vertical

(define (intersect s1 s2)
  (cond
    [(and (horizontal? s1) (not (horizontal? s2))) (intersect-normal s1 s2)]
    [(and (not (horizontal? s1)) (horizontal? s2)) (intersect-normal s2 s1)]
    [else #f]))

;; Required that hh is horizontal and s2 is vertical
(define (intersect-normal hh vv)
  (let ([left   (posn-x (segment-p1 hh))]
        [right  (posn-x (segment-p2 hh))]
        [y      (posn-y (segment-p1 hh))]
        [top    (posn-y (segment-p2 vv))]
        [bottom (posn-y (segment-p1 vv))]
        [x      (posn-x (segment-p1 vv))])
    (if (and (<= left x) (<= x right)
             (<= bottom y) (<= y top))
        (posn x y)
        #f)))

;; Lines
;; -----

;; A line is a list of at least two positions, each of which is vertically or
;; horizontally away from the last
(struct line (positions))

;; Convert a line to a list of ordered segments (in no particular order)
(define (line->segments ll)
  (define (segmentify p rest-of-line)
    (if (null? rest-of-line)
        null?
        (cons (make-ordered (segment p (car rest-of-line)))
              (segmentify (car rest-of-line) (cdr rest-of-line)))))
  (segmentify (car ll) (cdr ll)))


;; Displacements
;; -------------

;; A wire is a list of displacements, where a displacement is a direction and a
;; distance. A direction is one of 'up, 'down, 'left, or 'right
(struct displacement (dir dist) #:transparent)

;; parse-displacement : string? -> displacement?
(define (parse-displacement str)
  (let ([matches (regexp-match #rx"([UDLR])([0-9]+)" str)])
    (if matches
        (let ([dirn (cadr matches)]
              [dist (caddr matches)])
          (let ([dir (match dirn
                       ["U" 'up]
                       ["D" 'down]
                       ["L" 'left]
                       ["R" 'right])])
            (displacement dir (string->number dist))))
        (raise-user-error "Can't match displacement " str))))

;; wire-up : posn [list-of displacement] -> line
;; Convert a (non-empty) list of displacements into a line.
(define (wire-up start disps)
  (foldl ))


;; ------------------------------------------------------------
;; Tests

(module+ test
  (require rackunit))

(module+ test
  (check-true (posn<=? (posn 0 0) (posn 1 0)))
  (check-true (posn<=? (posn 0 0) (posn 0 1)))
  (check-false (posn<=? (posn 1 0) (posn 0 0)))
  (check-false (posn<=? (posn 0 1) (posn 0 0)))
  (check-false (posn<=? (posn 0 0) (posn 1 1))))

(module+ test
  (check-true (horizontal? (segment (posn 0 0) (posn 1 0))))
  (check-false (horizontal? (segment (posn 0 0) (posn 0 1)))))

(module+ test
  (check-equal? (intersect (segment (posn 0 5) (posn 5 5))
                           (segment (posn 2 0) (posn 2 10)))
                (posn 2 5)))
