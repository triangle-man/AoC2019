#lang racket

(module+ main

  (define *width* 25)
  (define *height* 6)
  
  (define instr
    (with-input-from-file "inputs/eight.txt"
      (thunk (read-line))))

  (define pxs (map (compose string->number ~a) (string->list instr)))

  (define layers
    (let loop ([pixels pxs])
      (if (null? pixels)
          null
          (let-values ([(layer remaining) (split-at pixels (* *width* *height*))])
            (cons layer (loop remaining))))))


  (define counts
    (map (λ (layer) (list (count (curry eq? 0) layer)
                          (count (curry eq? 1) layer)
                          (count (curry eq? 2) layer)))
         layers))


  ;; Part I
  
  (let ([min-zeros (argmin car counts)])
    (* (cadr min-zeros) (caddr min-zeros)))

  ;; Part II

  (define image
    (apply map list layers))
  
  (define flattened-image
    (map (λ (ps) (findf (compose not (curry eq? 2)) ps)) image))

  ;; Print image
  (let loop ([rows flattened-image])
    (if (null? rows)
        null
        (let-values ([(row rest) (split-at rows *width*)])
          (cons (row->string row) (loop rest)))))


  
  )

(define (row->string row)
  (list->string
   (map (λ (n) (cond
                 [(eq? n 0) #\space]
                 [(eq? n 1) #\#]))
        row)))


