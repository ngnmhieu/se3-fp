#lang racket

; Aufgabe 1
#|
Definitionen aus Aufgabe:

(define wuff 'Flocki )
(define Hund wuff )
(define Wolf 'wuff )
(define (welcherNameGiltWo PersonA PersonB)
(let ( ( PersonA 'Zaphod )
       (PersonC PersonA ) )
        PersonC ) )
(define xs1 '( 0 2 3 wuff Hund) )
(define xs2 (list wuff Hund) )
(define xs3 (cons Hund wuff ) )
_____________________________________________

1. wuff
-> 'Flocki
   , da die Funktion wuff zu 'Flocki evaluiert.
2. Hund
-> 'Flocki
   , da die Funktion "wuff" aufgerufen wird und dann wie in 1. zu 'Flocki evaluiert.
3. Wolf
-> 'wuff
   , der Wert 'wuff wird durch das Quotierungszeichen blockiert und kann somit nicht ausgewertet werden und wird wörtlich wieder gegeben.
4. (quote Hund)
-> 'Hund
   , durch die quote Funktion wird die Auswertung der Funktion blockiert und das Argument wörtlich wieder gegeben
5. (eval Wolf)
-> 'Flocki
   , der Ausdruck 'wuff, wird vom Evaluator ausgewertet. Er ruft also die Funktion wuff auf und gibt den Wert dieser Funktion aus.
6. (eval Hund)
-> Flocki: undefined
   , da sich hinter wuff das Symbol 'Flocki verbirgt und dieses versucht wird auszuwerten, aber es keine Funktion Flocki gibt kommt es zu einem Fehler.
7. (eval ’Wolf)
-> 'wuff
   , da (eval 'Wolf) = (Wolf) ist, wird der Wert der Funktion Wolf ausgegeben, dieser wird wegen dem Quotierungszeichen unausgewertet ausgegeben. 
8. (welcherNameGiltWo ’lily ’potter)
-> 'lily
   die Variablen des let-Blocks sind erst im Rumpf des let-Blocks zur Verfügung.
   Deshalb ist der Wert von PersonA beim Zuweisen auf PersonC noch der Funktion übergebene Wert.
9. (cdddr xs1)
-> '(wuff Hund)
   gibt eine Liste mit 0, 2, 3, 'wuff, 'Hund aus. (cdddr xs1) ist eine Kurzschreibweise für (cdr(cdr(cdr xs1))). Es werden so die erste drei Elemente der Liste entfernt.
   Übrig bleibt dann 'wuff, 'Hund.
10. (cdr xs2)
->  '(Flocki)
    xs2 ist eine Liste die aus der Werten der Konstanten wuff und Hund besteht. wuff ist eine Funktion mit dem Wert 'Flocki, dieser erste Wert wird aber durch cdr entfernt.
    Es bleibt Hund diese Funktion ruft wuff auf und somit auch 'Flocki.
11. (cdr xs3)
->  'Flocki
    xs3 ist ein Paar mit den werten der Konstanten Hund und wuff. Beide besitzen nach Auswertung die Werte 'Flocki.
    Durch cdr wird nur das zweite Element des Paares zurückgegeben.
12. (sqrt 1/4)
->  1/2
13. (eval ’(welcherNameGiltWo ’Wolf ’Hund))
->  'Wolf
    ???
14. (eval (welcherNameGiltWo ’Hund ’Wolf ))
->  'Flocki
    ???
|#

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
;; Berechnet r^n von r = rationale Zahl und  n = natürliche Zahl
(define (power r n)
  (if( zero? n) 1                     ; prüfen ob n = 0. Ja, dann Rückgabewert = 1 ->(r^0 = 1) 
    (if                               ; sonst...
      (odd? n)                        ; prüfen ob n ungerade Zahl ist
      (* r (power r (- n 1)))         ; ja, dann berechne r * r^(n-1)
      (sqr   (power r (/ n 2))))))    ; Wenn nein, dann berechne (r^(n/2))^2
;Prüfaufgaben
(~a "Prüfaufgaben 2.2: ")
(~a "0^0: " (power 0 0))
(~a "0^1: " (power 0 1))
(~a "0^10: " (power 0 10))
(~a "1^0: " (power 1 0))
(~a "20^0: " (power 20 0))
(~a "2^1: " (power 2 1))
(~a "2^2: " (power 2 2))
(~a "2^8: " (power 2 8))

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
