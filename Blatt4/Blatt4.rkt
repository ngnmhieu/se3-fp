#lang racket
(require "Blatt3.rkt")

; Aufgabe 1

; 1. (Bitte beachten Sie die besondere Form der Quotierung!)
(max (min 5 (- 8 7)) 6)
; -> 6
; das min von 5 und 8-7=1 ist Zwischenergebnis: 1. Das max von 1 und 6 ist das Endergebnis: 6.

; 2.
'(+ ,(- 13 11) 17)
; -> '(+ ,(- 13 11) 17)
; die Evaluierung wird durch das Quotierungszeichen blockiert. 

; 3.
(cadr '(Good King Wenceslas))
; -> King
; das zweite Element der Liste (Good King Wenceslas) ist "King". (cadr) gibt das zweite Element einer Liste zurück.

; 4.
(cdddr '(looked out (On the feast of Steven)))
; -> '()
; Der Ausdruck ist gleich bedeutend wie (cdr(cdr(cdr('(looked out (On the feast of Steven))))))
; Nachdem drei Elemente aus der Liste genommen werden, er gibt sich offensichtlich eine leere Liste

; 5.
(cons 'When '(the snow lay round about))
; -> '(When the snow lay round about)
; cons konstruiert eine Liste aus dem Element "When" (der Kopf der Liste) und der Liste (the snow lay round about) (der Körper der Liste.

; 6.
(cons '(Deep and) 'crisp)
; -> '((Deep and) . crisp)
; hier werden mit der cons-Funktion die beiden Elemente zu einem Paar (dotted pair) verknüpft.

; 7.
(equal? (list 'and 'even) '(and even))
; -> #t
; die Funktion list verknüpft Elemente beliebigen Typs zu einer Liste. Hier werden also verglichen:
; (list 'and 'even)-> '(and even) und '(and even).

; 8.
(eq? (list 'Rudolph 'the 'red-nosed 'reindeer)
     (cons 'Rudolph '(the 'red-nosed 'reindeer)))
; -> #f
; eq? gilt für identische Objekte. Die beiden Listen sind aber nicht identisch.
; (list 'Rudolph 'the 'red-nosed 'reindeer)->'(Rudolph the red-nosed reindeer) vs. '(Rudolph the 'red-nosed 'reindeer)<-(cons 'Rudolph '(the 'red-nosed 'reindeer))


; Aufgabe 2.1

; NOT_MELDUNG     := UEBERSCHRIFT STANDORT NOTFALL_ART HILFELEISTUNG PEILZEICHEN UNTERSCHRIFT "OVER"
; UEBERSCHRIFT    := UBERSCHRIFT_1 UBERSCHRIFT_2 UBERSCHRIFT_3
; UEBERSCHRIFT_1  := "MAYDAY" "MAYDAY" "MAYDAY" 
; UEBERSCHRIFT_2  := "HIER IST" | "DELTA ECHO"
; UEBERSCHRIFT_3  := SCHIFTNAME SCHIFTNAME SCHIFTNAME RUFZEICHEN_BUCH
; UEBERSCHRIFT_4  := "MAYDAY" SCHIFTNAME "ICH BUCHSTABIERE" SCHIFTNAME_BUCH
; UEBERSCHRIFT_5  := "RUFZEICHEN" RUFZEICHEN_BUCH
; STANDORT        := "NOTFALLPOSITION" SATZ
; NOTFALL_ART     := "NOTFALLART" SATZ 
; HILFELEISTUNG   := SATZ+
; PEILZEICHEN     := "--"
; UNTERSCHRIFT    := SCHIFTNAME RUFZEICHEN_BUCH
; 
; SCHIFTNAME      := ZEICHEN+
; SCHIFTNAME_BUCH := SEE_ZEICHEN+
; RUFZEICHEN_BUCH := SEE_ZEICHEN SEE_ZEICHEN SEE_ZEICHEN SEE_ZEICHEN
; SATZ            := (WORT | " ")+
; WORT            := ZEICHEN+
; ZEICHEN         := "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
;                  | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
;                  | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "." | ","
;                  | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
; SEE_ZEICHEN     := "Alpha" | "Bravo" | "Charlie" | "Delta" | "Echo"
;                  | "Foxtrot" | "Golf" | "Hotel" | "India" | "Juliet"
;                  | "Kilo" | "Lima" | "Mike" | "November" | "Oscar"
;                  | "Papa" | "Quebec" | "Romeo" | "Sierra" | "Tango"
;                  | "Uniform" | "Viktor" | "Whiskey" | "X-"ray | "Yankee" | "Zulu"
;                  | "Nadazero" | "Unaone" | "Duotwo" | "Terrathree" | "Carrefour"
;                  | "Pentafive" | "Soxisix" | "Setteseven" | "Oktoeight" | "Novonine"
;                  | "Decimal" | "Stop"

; Aufgabe 2.2
(define (list->string xs)
  (if (empty? (cdr xs)) (car xs) (string-append (car xs) " " (list->string (cdr xs)))))
(define (generate-not-meldung schiffname rufzeichen position notfallart angaben)
  (let* ([rufzeichen_buch (list->string (codierung rufzeichen))]
         [ueberschrift (string-append "MAYDAY MAYDAY MAYDAY\n"
                                      "DELTA ECHO\n"
                                      schiffname " " schiffname " " schiffname " " rufzeichen_buch "\n"
                                      "MAYDAY " schiffname " ICH BUCHSTABIERE " (list->string (codierung schiffname)) "\n"
                                      "RUFZEICHEN " rufzeichen_buch)]
         [unterschrift (string-append schiffname " " rufzeichen_buch)]
         [position (string-append "NOTFALLPOSITION " position)]
         [art (string-append "NOTFALLART " notfallart)])
    (string-append ueberschrift "\n" position "\n" art "\n" angaben " --\n" unterschrift "\nOVER")))

(display "\n\nNotmeldung fuer UNICORN\n")
(display "-----------------------------------------\n")
(display (generate-not-meldung "UNICORN" "UNCR" "UNGEFAEHR 5 SM NORDWESTLICH LEUCHTTURM ROTER SAND" "SCHLIMM"
                                "SCHWERE SCHLAGSEITE WIR SINKEN\nKEINE VERLETZTEN\nSECHS MANN GEHEN IN DIE RETTUNGSINSEL\nSCHNELLE HILFE ERFORDERLICH\nICH SENDE DEN TRÄGER"))
(display "\n\nNotmeldung fuer NAUTILUS\n")
(display "-----------------------------------------\n")
(display (generate-not-meldung "NAUTILUS" "DEYJ" "UNGEFAEHR 10 sm OESTLICH POINT NEMO 48° 52’ 31,75” S, 123° 23’ 33,07“ WD" "Ein Riesenkrake hat das Schiff umschlungen, ein grosses Leck im Rumpf"
                                "20 Personen an Bord, treiben antriebslos an der Wasseroberflaeche"))
(display "\n\nNotmeldung fuer Maltese Falcon\n")
(display "-----------------------------------------\n")
(display (generate-not-meldung "MALTESEFALCON" "HUQ9" "N 54° 34’ 5,87”, E 8° 27’ 33,41”" "Auf eine Sandbank aufgelaufen"
                                "10 Mann an Bord, das Schiff ist 88m lang, schwarzer Rumpf. Unfallzeit 0730 UTC"))
; Aufgabe 2.3


;AUFGABE 3.1
; Erkären Sie den Unterschied zwischen innerer Reduktion und äußerer Reduktion:

; bei der inneren Reduktion werden die Terme von innen nach außen reduziert, somit werden zuerst die inneren Klammern und Funktionen ausgewertet und erst später die äußeren.
; bei der äußeren Redunktion werden die Terme von außen nach innen reduziert, somit werden zuerst die äußeren Funktionen und Klammern ausgewertet und erst später die inneren.

;zeigen Sie beispielhaft die Auswertung des Ausdrucks mit beiden Reduktionsstrategien: 
;(hoch3 (+ 3 (hoch3 3)))
;mit
;(define (hoch3 x) (* x x x ))

;innere Reduktion:
;-> (hoch3 (+ 3 (hoch3 3)))
;-> (hoch3 (+ 3 (* 3 3 3)))
;-> (hoch3 (+ 3 27))
;-> (hoch3 30)
;-> (* 30 30 30)
;-> 27000

;äußere Reduktion:
;-> (hoch3 (+ 3 (hoch3 3)))
;-> (* (+ 3 (hoch3 3)) (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
;-> (* (+ 3 (* 3 3 3)) (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
;-> (* (+ 3 27) (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
;-> (* 30 (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
;-> (* 30 (+ 3 (* 3 3 3)) (+ 3 (hoch3 3)))
;-> (* 30 (+ 3 27) (+ 3 (hoch3 3)))
;-> (* 30 30 (+ 3 (hoch3 3)))
;-> (* 30 30 (+ 3 (* 3 3 3)))
;-> (* 30 30 (+ 3 27))
;-> (* 30 30 30)
;-> 27000

;AUFGABE 3.2
;Welche Reduktionsstrategie wird in Racket für Funktionen angewendetund welche für Spezialformen:
; In Racket wird die innere Reduktion auf Funktionen angewandt. Spezialformen werden allerdings von links nach rechts, mit der äußeren Reduktion ausgewertet.


;AUFGABE 3.3
; Bei selbstgeschriebenen Funktionen werden diese wie in 3.2 beschrieben nach der inneren Reduktion ausgewertet. In dem angegebenen Beispiel wird mit dem
; new-if eine condition geprüft. Allerdings ,durch die innere Reduktion, erst nachdem "else-clause" ausgeührt wurde. Das if ist dafür da bestimmte
; Fälle möglichst schnell abzufangen und ein Ergebnis zu liefern. Bei der eigens defineirten new-if Funktion ist unnötiger Rechenaufwand vorhanden. Dashalb sind
; für bestimmte Kontrollstrukturen Spezialformen sehr hilfreich.
