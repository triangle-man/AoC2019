#lang racket

(module+ main
  
  (define inputs
    (with-input-from-file "inputs/six.txt"
      (thunk
       (for/list ([line (in-lines)])
         (match (regexp-match #rx"(...)\\)(...)" line)
           [(list _ left right) (cons left right)]
           [else (raise-user-error "Unexpected input")])))))
  
  (define orbit-map (tree-grow inputs "COM"))

  ;; Part I
  (println (apply + (flatten (heights orbit-map))))  
  
  )

(module+ test
  (require rackunit)
  
  (define eg1
    '(("COM" . "B")
      ("B" . "C")
      ("C" . "D")
      ("D" . "E")
      ("E" . "F")
      ("B" . "G")
      ("G" . "H")
      ("D" . "I")
      ("E" . "J")
      ("J" . "K")
      ("K" . "L")))

  (define tree1 (tree-grow eg1 "COM"))

  (check-eq?
   42
   (apply + (flatten (heights tree1))))
  
  )

;; Convert a list of pairs to a hashtable with lists as values
(define (parse-input in)
  (make-immutable-hash
   (map ;; A group is a list of pairs with the same car
      (λ (gp) (cons (caar gp) (map cdr gp)))
      (group-by car in))))

;; A /tree/ is either
;; - a string (eg, "COM"); or
;; - a pair of a string and a list (possibly empty) of trees
;; such that no two strings are the same

;; A bough? is a pair of nodes
;; tree-grow : boughs? leaf-node? -> tree?
(define (tree-grow boughs node)
  (tree-grow+ (parse-input boughs) node))

(define (tree-grow+ bough# node)
  (let ([shoots (hash-ref bough# node #f)])
    (if (not shoots)
        node
        (cons node (map (curry tree-grow+ bough#) shoots)))))

;; Convert each node to its height
(define (heights tree)
  (heights+ tree 0))

(define (heights+ tree h)
  (if (pair? tree)
      (cons h (map (curryr heights+ (+ h 1)) (cdr tree)))
      h))

;; Find the path from the root to a given leaf
(define (path-to-leaf tree node)
  #f)
