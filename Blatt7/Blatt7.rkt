#lang racket

(require 2htdp/image)

; Aufgabe 1

(define (zaehlen-allgemein x xs)
  (if (empty? xs) 0 ; 0 falls wenn liste leer
    (let ([c (if (equal? x (car xs)) 1 0)]) ; falls x == erstes Element zaehlt 1 sonst 0
      (+ c (zaehlen-allgemein x (cdr xs)))))) ; addiert c zu das Ergebnis der Restliste
; Testdaten
(display "Test zahlen-allgemein\n")
(equal? 2 (zaehlen-allgemein 1 '(1 2 3 4 5 6 1)))
(equal? 0 (zaehlen-allgemein 1 '(2 3 4 5 6)))
(equal? 0 (zaehlen-allgemein 1 '()))

(define (zaehlen-endrek x xs)
  (letrec ([zaehlen (lambda (x xs acc)
      (if (empty? xs) acc ; Endergebnis zurueckgeben falls Liste leer
        (zaehlen x (cdr xs) ; ruft zahlen rekursiv mit dem neuen Wert von acc
                 (+ acc (if (equal? x (car xs)) 1 0)))))])
    (zaehlen x xs 0)))
; Testdaten
(display "Test zahlen-endrek\n")
(equal? 2 (zaehlen-endrek 1 '(1 2 3 4 5 6 1)))
(equal? 1 (zaehlen-endrek 1 '(1 2 3 4 5 6)))
(equal? 0 (zaehlen-endrek 1 '()))

; f ist ein Lambda der ein Element und eine Liste aufnimmt und gibt #t oder #f zurueck.
; TODO: benutze Funktion höhere Ordnung in der Vorlesung
; (define (zaehlen-custom f x xs)
;   (letrec ([zaehlen (lambda (x xs acc)
;       (if (empty? xs) acc ; Endergebnis zurueckgeben falls Liste leer
;         (zaehlen x (cdr xs) ; ruft zahlen rekursiv mit dem neuen Wert von acc
;                  (+ acc (if (f x (car xs)) 1 0)))))])
;     (zaehlen x xs 0)))
; Testdaten
(display "Test zahlen-custom\n")
; (equal? 2 (zaehlen-custom equal? 1 '(1 2 3 4 5 6 1)))
; (equal? 2 (zaehlen-custom > 3 '(1 2 3 4 5 6)))
; (equal? 3 (zaehlen-custom < 3 '(1 2 3 4 5 6)))

; Aufgabe 2

; N ist die Größe des Spiels (Konstante)
(define N 30)

; Aufgabe 2.1

; Wir überlegen uns die möglichen Operationen die an der Datenstruktur
; durchgeführt werden können.  Der direkte Zugriff auf die einzelnen Zellen ist
; die meist genutzte Operation - es muss daher schnell sein.  Darüber hinaus
; möchten wir für eine bestimmte Zelle auf ihre Nachbarn schnell zugreifen
; können.  Die Zustände der einzelnen Zellen muss während des Spiels geändert
; werden, um neue Generation zu erzeugen.
;
; Anhand der obigen Überlegungen liegen uns einige Möglichkeiten vor, mit denen
; wir unser Spiel modellieren können:
;
; 1. Eine Liste von Listen:
;    Jede Unterliste stellt eine Zeile dar, in der jedes Element eine Zelle darstellen.
;
; 2. Ein Vektor von Vektoren:
;    Ähnlich wie die 1. Variant wo jeder Untervektor eine Zeile representiert.
;
; 3. Ein (ein-dimensionaler) Vektor:
;    Alle Zellen werden in einem linearen Vektor abgelegt, dabei wird der Vektor
;    in N Teilen aufgeteilt, die jeweils N Elemente haben. Der Index eines Elements
;    wird aus zwei Koordinaten berechnet. Diese Darstellung ähnelt die 2D-Array
;    Representation in herkömmlichen Programmiersprachen wie C oder Java.
;
; Das 2. Variant ist im Vergleich zu dem 1. besser, weil man auf die Elemente eines Vektors
; direkt zugreifen kann und nicht durch die Liste durchgehen muss. Beim 2. Variant benötigt man
; aber immer noch mehrere Operationen, um auf eine Zelle zuzugreifen. Man muss zunächst auf den
; Untervektor zugreifen und erst dann auf die Zelle zugreifen.
; 
; Beim 3. Variant lässt sich in einem Schritt auf eine Zelle zugreifen (mit einem minimalen Aufwand
; für die Berechnung des Indizes). Außerdem ist der Zugriff auf die Nachbarn-Zellen ggf. auch einfacher.
;
; Die Darstellung mit einem linearen Vektor (3. Variant) ist jedoch einfacher, aber wir bevorzugen
; das 2. Variant mit einem Vektor von Vektoren, denn es macht Aufteilung der Zeilen sowie 
; die Darstellung von dem Spiel viel einfacher als das 3. Variant.

; Damit werden wir einen Vektor von Vektoren als die Datenstruktur für unser Programm nutzen.
; Jedes Element enhält einen booleschen Wert: #t heißt lebendige Zelleund #f heißt tote Zelle.

; Aufgabe 2.2
; Konstante zur Konfiguration der Visualisierung
(define DISPLAY_SIZE 500)

; display-spiel visualisiert das Spiel 
; @param spiel ist einen Vektor länger NxN
(define (display-spiel spiel)
  (let* ([display-zelle (lambda (zelle)
                          (if zelle
                            (rectangle 30 30 "solid"   "black")
                            (rectangle 30 30 "outline" "black")))]
         [display-zeile (lambda (zeile)
                          (apply beside
                            (vector->list (vector-map display-zelle zeile))))])
  (apply above (vector->list (vector-map display-zeile spiel)))))

; Testdaten
(display "Test display-spiel\n")
(define sample-spiel
  #(#(#f #t #f #f #f #f #f #f #f #f)
    #(#f #f #t #f #f #f #f #f #f #f)
    #(#t #t #t #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)))
(display-spiel sample-spiel)


 Aufgabe 2.3
; eine Funktion die für einen beliebigen Index des Spielzustandes die Werte der Nachbarschaft ermittelt
(define (WerteDerNachbarn vector y x)
   (if (>= 0 x y 9) '()
       (Auswerten
        (list
         (vector-ref (vector-ref vector (- y 1)) (- x 1))
         (vector-ref (vector-ref vector (- y 1)) x)
         (vector-ref (vector-ref vector (- y 1)) (+ x 1))
         (vector-ref (vector-ref vector y) (+ x 1))
         (vector-ref (vector-ref vector (+ y 1)) (+ x 1))
         (vector-ref (vector-ref vector (+ y 1)) x)
         (vector-ref (vector-ref vector (+ y 1)) (- x 1))
         (vector-ref (vector-ref vector y) (- x 1))))))    

; anhand der WerteDerNAchbarn den Folgezustand bestimmen: schwarz oder umrandet.
(define (Auswerten list)
  (if (null? list) '()
      (if (= 3 (count identity list)) ; ToDo: dann lebt Zelle weiter
          (if (<= (count identity list)2) ; ToDo: dann stirbt Zelle
              (if (>= (count identity list) 4) ; ToDo: dann stirbt Zelle
