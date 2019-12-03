#lang racket

;; Data types:
;; posn         : an (x, y) pair
;; segment      : a posn--posn pair
;; line         : a list of positions
;; displacement : a posn and a compass direction 

(module+ main

  ;; Read and parse input wires
  (define (parse-input-line str)
    (map parse-displacement (string-split str ",")))
  
  (define-values (in1 in2)
    (with-input-from-file "inputs/three.txt"
      (λ () (values (parse-input-line (read-line))
                    (parse-input-line (read-line))))))

  (define wire1
    (line->segments (wire-up (posn 0 0) in1)))
  (define wire2
    (line->segments (wire-up (posn 0 0) in2)))

  ;; The intersection points, excluding the origin
  (define is (cdr (intersect-segments wire1 wire2)))

  ;; Part I
  (println
   (apply min (map posn-manhatten-norm is)))

  ;; Part II
  (define counts1 (map (λ (p) (line-count-steps wire1 p)) is))
  (define counts2 (map (λ (p) (line-count-steps wire2 p)) is))

  (println
   (apply min (map + counts1 counts2)))

  )


;; Positions
;; ---------

;; A posn is two integer coordinates:
(struct posn (x y) #:transparent)

;; Two positions are ordered if the first is not above or to the right of the
;; other
(define (posn<=? p1 p2)
  (and (<= (posn-y p1) (posn-y p2))
       (<= (posn-x p1) (posn-x p2))))

(define (posn-manhatten-norm p)
  (+ (abs (posn-x p)) (abs (posn-y p))))


;; Segments
;; --------

;; A segment is a pair of posn's.
(struct segment (p1 p2) #:transparent) ;; such that (posn<=? p1 p2) is #t

(define (horizontal? seg)
  (= (posn-y (segment-p1 seg)) (posn-y (segment-p2 seg))))

(define (segment-length seg)
  (let ([p1 (segment-p1 seg)]
        [p2 (segment-p2 seg)])
    (+ (abs (- (posn-x p2) (posn-x p1)))
       (abs (- (posn-y p2) (posn-y p1))))))

;; A segment is ordered if its two points are ordered
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

;; Given two lists of segments, find the intersections of horizontal segments
;; from one with vertical segments from the other
(define (intersect-segments ss1 ss2)
  (let ([ordered-ss1 (map make-ordered ss1)]
        [ordered-ss2 (map make-ordered ss2)])
   (filter values
           (for*/list ([s1 ordered-ss1]
                       [s2 ordered-ss2])
             (intersect s1 s2)))))

;; Given a segment (not necessarily ordered) and a position, decide whether the
;; point lies on the segment. If it does, return the number of points between
;; the start of the segment and this position (inclusive); otherwise return #f
(define (segment-locate-posn seg pos)
  (if (horizontal? seg)
      (and (= (posn-y pos) (posn-y (segment-p1 seg)))
           (locate-within (posn-x (segment-p1 seg))
                          (posn-x (segment-p2 seg))
                          (posn-x pos)))
      (and (= (posn-x pos) (posn-x (segment-p1 seg)))
           (locate-within (posn-y (segment-p1 seg))
                          (posn-y (segment-p2 seg))
                          (posn-y pos)))))

;; Does x lie within x1 and x2, and if so, how far from x1?
(define (locate-within x1 x2 x)
  (or
   (and (<= x1 x) (<= x x2) (- x x1))
   (and (<= x2 x) (<= x x1) (- x1 x))))

;; Count how many steps from the beginning we find the position
(define (line-count-steps ss pos)
  (define (count-steps segments-remaining steps-so-far)
    (if (null? segments-remaining)
        steps-so-far
        (let* ([next-segment (car segments-remaining)]
               [N-steps (segment-locate-posn next-segment pos)])
          (if N-steps ;; found intersection
              (+ steps-so-far N-steps)
              (count-steps (cdr segments-remaining)
                           (+ steps-so-far (segment-length next-segment)))))))
  (count-steps ss 0))





;; Lines
;; -----

;; A line is a list of at least two positions, each of which is vertically or
;; horizontally away from the last
(struct line (positions))

;; Convert a line to a list of segments (in no particular order)
(define (line->segments ll)
  (define (segmentify p rest-of-line)
    (if (null? rest-of-line)
        null
        (cons (segment p (car rest-of-line))
              (segmentify (car rest-of-line) (cdr rest-of-line)))))
  (segmentify (car ll) (cdr ll)))


;; Displacements
;; -------------

;; A displacement is a direction and a distance. A direction is one of 'up,
;; 'down, 'left, or 'right
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

(define (displace p delta)
  (let ([dist (displacement-dist delta)])
   (match (displacement-dir delta)
     ['up    (struct-copy posn p (y (+ (posn-y p) dist)))]
     ['down  (struct-copy posn p (y (- (posn-y p) dist)))]
     ['left  (struct-copy posn p (x (- (posn-x p) dist)))]
     ['right (struct-copy posn p (x (+ (posn-x p) dist)))])))

;; wire-up : posn [list-of displacement] -> line
;; Convert a (non-empty) list of displacements into a line.
(define (wire-up origin deltas)
  (define (wire-up-helper line-so-far deltas-remaining)
    (if (null? deltas-remaining)
          line-so-far
          (wire-up-helper
           (cons (displace (car line-so-far) (car deltas-remaining))
                 line-so-far)
           (cdr deltas-remaining))))
  (reverse (wire-up-helper (list origin) deltas)))

;; ------------------------------------------------------------
;; Tests

(module+ test
  (require rackunit))

(module+ test
  (check-true (posn<=? (posn 0 0) (posn 1 0)))
  (check-true (posn<=? (posn 0 0) (posn 0 1)))
  (check-false (posn<=? (posn 1 0) (posn 0 0)))
  (check-false (posn<=? (posn 0 1) (posn 0 0))))

(module+ test
  (check-true (horizontal? (segment (posn 0 0) (posn 1 0))))
  (check-false (horizontal? (segment (posn 0 0) (posn 0 1)))))

(module+ test
  (check-equal? (intersect (segment (posn 0 5) (posn 5 5))
                           (segment (posn 2 0) (posn 2 10)))
                (posn 2 5)))

(module+ test
  
  (define (make-eg str)
    (line->segments
     (wire-up (posn 0 0)
              (map parse-displacement
                   (string-split str ",")))))

  (define eg0a (make-eg "R8,U5,L5,D3"))
  (define eg0b (make-eg "U7,R6,D4,L4"))
  
  (define eg1a
    (make-eg "R75,D30,R83,U83,L12,D49,R71,U7,L72"))

  (define eg1b
    (make-eg "U62,R66,U55,R34,D71,R55,D58,R83"))

  (check-equal?
   (apply min (map posn-manhatten-norm (cdr (intersect-segments eg1a eg1b))))
   159)

  (define eg2a
    (make-eg "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"))

  (define eg2b
    (make-eg "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))

  
