#lang racket 

; (require se3-bib/setkarten-module)

; Aufgabe 1.1
; Eine Funktion eine Funktion höherer Ordnung, wenn sie Funktionen als
; Argumente erhält oder Funktion als Rückgabewert hat.
; Aufgabe 1.2
; a) `gerade-oder-ungegrade` ist keine Funktion höherer Ordnung.
; b) `map` ist eine Funktion höherer Ordung. `map` erhält als Argumente eine
;    Liste und eine Funktion und gibt eine Liste zurück, deren Elemente durch die
;    Anwendung der Funktion auf das entsprechende Element in der originalen Liste.
; c) `erstelle-assoziationsliste` ist keine Funktion höherer Ordnung.
; d) `ermittle-vergleichsoperation` gibt eine Funktion zurück und ist daher eine Funktion
;    höherer Ordnung.
; e) `schweinchen-in-der-mitte` ist eine Funktion höherer Ordnung, denn sie nimmt eine Funktion als
;    Argument und gibt eine andere Funktion zurück.

; Aufgabe 1.3
; Bei dem Aufruf:
; ((schweinchen-in-der-mitte list 4) 1 3)
; Bei der Auswertung des inneren Ausdrucks `(schweinchen-in-der-mitte list 4)`,
; werden die Variablen in der Umgebung von `schweinchen-in-der-mitte`, `f` und
; `arg1` jeweils an der Funktion `list` und dem Wert 4 gebunden.
; Die Umgebung des Lambda-Ausdrucks ist dann `f`, `arg1`, `arg2`, `arg3`, wobei
; `f` und `arg1` fest an den Werten `list` und 4 gebunden, `arg2` und `arg3` werden
; erst beim Aufruf dieses Funktion an den Werten 1 und 3 gebunden.

; Aufgabe 1.4
; (foldl (curry + 2) 3 '(3 4 5))
;   Ergebnis: 21.
;   `(curry + 2)` liefert eine Addition-Funktion zurück, die bei jedem Aufruf 2
;   zu dem Ergebnis addiert.
;   `foldl` wendet die dann an allen Elementen der Liste zusammen mit dem
;   Initialwert, liefert als Ergebnis 21.
;
; (map gerade−oder−ungerade '(4 587 74 69 969 97 459 4))
;   Ergebnis: '(ungerade gerade ungerade gerade gerade gerade gerade ungerade)
;   `map` geht die Liste durch, wendet die Funktion `gerade-oder-ungerade`
;   auf jedes Element in der Liste an und gibt eine neue Liste zurück mit
;   den Ergebnissen. `gerade−oder−ungerade` liefert 'gerade zurück falls eine
;   Zahl ungerade ist und umgekehrt.
;
; (filter number? '((a b) () 1 (()) 4 -7 "a"))
;   Ergebnis: '(1 4 -7)
;   `filter` geht die Liste durch und wendet das Prädikat `number?` auf die Elemente
;   an. Eine Liste wird zurückgeliefert mit der Elemente, die dazu führt, dass `number?`
;   zu #t ausgewertet ist, nämlich die Zahlen.
;
; ((compose (curry foldl + 0) (curry filter (curryr < 0))) '(5682 48 24915 -45 -6 48))
;   1. (curry (foldl + 0))         -> Addiert alle Zahlen in der Liste
;   2. (curry filter (curryr < 0)) -> Funktion die alle negativen Zahlen filtert.
;   `compose` kombiniert dann die zwei Funktionen. Die letzte Funktion wird
;   erst ausgeführt mit der Liste als Argument. Das Ergebnis davon dient als
;   Argument für den Aufruf der ersten Funktion.
;   Ergebnis: -51

; Aufgabe 2.1
(define (square-list xs) (map (lambda (x) (* x x)) xs))
; Testdaten
(display "\n## Aufgabe 2.3 ##\n")
(display "square-list: \n")
(square-list '())
(square-list '(5))
(square-list '(1 2 3 4))

; Aufgabe 2.2
(define (teilbar-9-oder-11 xs)
  (let ([teilbar (lambda (x y) (equal? 0 (modulo x y)))])
    (filter
      (lambda (x) (or (teilbar x 11) (teilbar x 9))) xs)))
; Testdaten
(display "\n## Aufgabe 2.3 ##\n")
(display "teilbar-9-oder-11: \n")
(teilbar-9-oder-11 '())
(teilbar-9-oder-11 '(9 1 2 3 4))
(teilbar-9-oder-11 '(11 12 13 14))
(teilbar-9-oder-11 '(0 9 11 99 1089))

; Aufgabe 2.3
(define (ungerade-groesser-6 xs)
  ((compose
    (curry foldl + 0)
    (curry filter (curry < 6))
    (curry filter odd?)) xs))
; Testdaten
(display "\n## Aufgabe 2.3 ##\n")
(display "ungerade-groesser-6: \n")
(ungerade-groesser-6 '(1 2 3 4 5 6 7 8 9 10 11 12))

; Aufgabe 2.4
(define (list-split f xs)
  (list
    (filter f xs) 
    (filter (compose not f) xs)))

(display "\n## Aufgabe 2.4 ##\n")
(display "list-split: \n")
(list-split odd? '(0 1 2 3 4 5 6 7 8 9))
(list-split even? '(0 1 2 3 4 5 6 7 8 9))

; Aufgabe 3.1

; Verfuegbare Eigenschaften
(define Number  '(1 2 3))
(define Pattern '(waves oval rectangle))
(define Mode    '(outline solid hatched))
(define Color   '(red green blue))

; Repräsentation einer Spielkarte
; Funktionen höherer Ordnung lassen sich leichter
; mit Listen anwenden. Die Representation der Eigenschaften
; einer Spielkarte ist also eine Liste mit folgender Struktur:
; '(Number Pattern Mode Color)
(define sample-cards '(3 waves outline red))

; Aufgabe 3.2
(define all-cards
  ; for each element in xs addCombinationOf generates
  ; all combinations of that element with the given
  ; list of objects
  (let ([addCombinationOf
          (lambda (objects xs)
            (foldl (lambda (partial result) ; iterate over all elements
                     (append result         ; generate new elements with the objects
                             (map (lambda (n) (cons n partial)) objects))) '() xs))])
    (addCombinationOf
      Number
      (addCombinationOf
        Pattern
        (addCombinationOf
          Mode
          (addCombinationOf Color (list '())))))))

(display "\n## Aufgabe 3.2 ##\n")
(~a "All cards (" (length all-cards) "): " all-cards)

; Aufgabe 3.3
(define (member? x xs)
  (if (or (null? xs)
          (empty? xs)) #f
      (or (equal? x (car xs)) (member? x (cdr xs)))))

(define (is-a-set? card) (letrec ([all-same-or-diff?  (lambda (x y z acc)
               (and acc
                    (or (and (equal? x y) (equal? y z))
                        (not (or (equal? x y) (equal? y z) (equal? x z))))))])
    (if (not (and
               (list? card)
               (equal? 3 (length card))
               (member? (car card) all-cards)
               (member? (caar card) all-cards)
               (member? (caaar card) all-cards)
               )) #f
    (foldl all-same-or-diff? #t (car card) (cadr card) (caddr card)))))

; Testdaten
(display "\n## Aufgabe 3.3 ##\n")
(define test-set '((2 red oval hatched) 
                   (2 red rectangle hatched)
                   (2 red wave hatched)))

(display "is-a-set? should return #t: ")
(is-a-set? '((2 red oval hatched) 
             (2 red rectangle hatched)
             (2 red wave hatched)))
(display "is-a-set? should return #f: ")
(is-a-set? '((2 red rectangle outline) 
             (2 green rectangle outline)
             (1 green wave solid)))

; Aufgabe 3.4

