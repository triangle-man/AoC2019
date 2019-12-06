#lang racket

(define inputs
  (with-input-from-file "inputs/six.txt"
    (thunk
     (for/list ([line (in-lines)])
       (match (regexp-match #rx"(...)\\)(...)" line)
         [(list _ left right) (cons left right)]
         [else (raise-user-error "Unexpected input")])))))

;; A /tree/ is either
;; - a string (eg, "COM"); or
;; - a pair of a string and a list (possibly empty) of trees
;; such that no two strings are the same

;; A /bough/ is a pair of strings

;; Plan (not very efficient)
;; Starting with a root tree,
;; scan through the list of remaining boughs
;; attempt to add the bough to the leaves of the root tree
;; repeat.

;; tree-add-bough : tree? bough? -> tree? or #f 
(define (tree-add-bough tree bough)
  (if (not (pair? tree))
      (if (equal? tree (car bough))
          (cons tree (list (cdr bough)))
          #f)
      (let* ([trees     (cdr tree)]
             [new-trees (map (curryr tree-add-bough bough) trees)])
        (if (or new-trees)
            (zip/or new-trees trees)
            #f))))

;; zip using or
(define (zip/or l1 l2)
  (map (λ (x y) (or x y)) l1 l2))
