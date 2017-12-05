#lang racket

; Aufgabe 1.1

; In der Funktion `take` und `drop` liegt eine lineare Rekursionn vor denn es gibt nur
; einen Rekursionsaufruf in der Definition. Es gibt keine minimale Rekursion,
; weil man zwei Basisfälle.

; In der Funktion `merge` liegt eine lineare Rekursion vor. In der letzten
; Fallunterscheidungen wird jeweils ein Rekursionsaufruf verwendet.

; Die `merge-sort` Funktion hat eine baumartige Rekursion. Die baumartige
; Rekursion sieht man dadurch, dass `merge-sort` mehrfach rekursiv aufgerufen
; wird. Es könnte auch um eine geschachtelte Rekursion gehen: die Ergebnisse der
; Rekursivaufrufe von `take` `drop` sowie `merge-sort` werden als Argumente von
; dem Rekursionsaufruf von `merge` genutzt.

; Aufgabe 1.2
; `merge` und `merge-sort` nehmen als Argument eine Funktion auf (`rel<?`).
; `rel<?` wird benutzt in `merge` um die Elementen miteinander zu vergleichen.

; Aufgabe 1.3

(define (take n xs)
    (letrec
      ([takehilfe (lambda (n xs acc)
                    (cond
                      ((= 0 n) acc)
                      ((empty? xs) acc)
                      (else (takehilfe (- n 1) (cdr xs) (cons (car xs) acc)))))])
      (takehilfe n xs '())))
; Testdaten
(take 5 '(1 2 3 4 5 6))
(take 1 '(1 2))
(take 2 '())
(take 10 null)
