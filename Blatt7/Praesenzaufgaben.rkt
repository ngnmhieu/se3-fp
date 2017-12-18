#lang racket 

(define (laenge xs)
  (foldl (lambda (xs s) (+ 1 s)) 0 xs))
(laenge '(1 2 3 4 5))

(define (teile xs)
  (list (filter even? xs) (filter odd? xs)))
(teile '(1 2 3 4 5 6))

(define (mittelwert-1 xs)
  (/ (foldl + 0 xs) (length xs)))
(mittelwert-1 '(2 2 4 4))
