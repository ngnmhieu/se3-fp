#lang racket

(require 2htdp/image)
(require 2htdp/universe)

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
(define N 10)

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
(define glieder-spiel
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
(display-spiel glieder-spiel)

(define simple-spiel
  #(#(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #t #t #t #f #f #f)
    #(#f #f #f #t #t #t #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)
    #(#f #f #f #f #f #f #f #f #f #f)))
(display-spiel glieder-spiel)


; Aufgabe 2.3
(define (get-zelle spiel y x)  ; den Wert der Zelle zurueckliefert, falls es nicht im Feld, #f
  (if (and (<= 0 x (- N 1)) (<= 0 y (- N 1)))
    (vector-ref (vector-ref spiel y) x)
    #f))

; eine Funktion die für einen beliebigen Index des Spielzustandes die Werte der Nachbarschaft ermittelt
(define (nachbarn-werte spiel y x)
  (if (>= 0 x y N) '()
    (list
      (get-zelle spiel (- y 1) (- x 1))
      (get-zelle spiel (- y 1)    x   )
      (get-zelle spiel (- y 1) (+ x 1))
      (get-zelle spiel    y    (+ x 1))
      (get-zelle spiel (+ y 1) (+ x 1))
      (get-zelle spiel (+ y 1)    x   )
      (get-zelle spiel (+ y 1) (- x 1))
      (get-zelle spiel    y    (- x 1)))))
; Testdaten
(~a "(nachbarn-werte glieder-spiel 0 0): " (nachbarn-werte glieder-spiel 0 0))
(~a "(nachbarn-werte glieder-spiel 1 0): " (nachbarn-werte glieder-spiel 1 0))
(~a "(nachbarn-werte glieder-spiel 2 2): " (nachbarn-werte glieder-spiel 2 2))


; eine Funktion, die die Werte dieser 8er-Nachbarschaft erhält, und ge- mäß den Spielregeln den Folgezustand des Automaten am übergebenen Index bestimmt
(define (lebt-oder-tot spiel y x)
  (let ([lebende (count identity (nachbarn-werte spiel y x))])
    (cond
      [(equal? 3 lebende) #t]
      [(< lebende 2) #f]
      [(> lebende 3) #f]
      [else (get-zelle spiel y x)])))

; Testdaten
(~a "(lebt-oder-tot glieder-spiel 0 0): " (lebt-oder-tot glieder-spiel 0 0))
(~a "(lebt-oder-tot glieder-spiel 1 0): " (lebt-oder-tot glieder-spiel 1 0))
(~a "(lebt-oder-tot glieder-spiel 2 2): " (lebt-oder-tot glieder-spiel 2 2))

(define (next-spiel spiel)
  (let ([new-zeile
          (lambda (y)
            (list->vector (map
              (lambda (x) (lebt-oder-tot spiel y x))
              (range 0 N))))])
  (list->vector (map new-zeile (range 0 N)))))

(big-bang glieder-spiel
          (to-draw display-spiel)
          (on-tick next-spiel 0.25))

(big-bang simple-spiel
          (to-draw display-spiel)
          (on-tick next-spiel 0.25))

