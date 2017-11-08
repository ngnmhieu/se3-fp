#lang racket

; Aufgabe 1


; Aufgabe 2.1
(define (falkultaet n)
  (if (= 0 n) 1 (* n (falkultaet (- n 1)))))
(~a "0! = " (falkultaet 0))
(~a "1! = " (falkultaet 1))
(~a "2! = " (falkultaet 2))
(~a "3! = " (falkultaet 3))
(~a "4! = " (falkultaet 4))
(~a "5! = " (falkultaet 5))
(~a "6! = " (falkultaet 6))
(~a "7! = " (falkultaet 7))
(~a "8! = " (falkultaet 8))
(~a "9! = " (falkultaet 9))
(~a "10! = " (falkultaet 10))

; Aufgabe 2.2


; Aufgabe 2.3
(define euler (letrec
                  ([schranke (/ 1 (expt 10 1000))]
                   [_euler (lambda (acc n)
                             (let* ([term (/ (+ n 1) (falkultaet n))]
                                    [sum (+ acc term)])
                               (if (< term schranke) sum (_euler sum (+ n 1)))))])
                (/ (_euler 0 0) 2)))

(~a "Erste 1000 Stellen der Euler-Zahl ")
(* euler (expt 10 1000))
; Aufgabe 2.4



; Aufgabe 3