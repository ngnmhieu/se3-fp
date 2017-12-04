#lang racket

(require se3-bib/butterfly-module)

(define-struct butterfly (fluegelfarbe musterung fuehlerform fluegelform))

; Datenstruktur zur Speicherung von Merkmaltypen
(define Musterungen '(star dots stripes))
(define Fluegelfarben '(blue green yellow red))
(define Fuehlerformen '(curved curly straight))
(define Fluegelformen '(ellipse rhomb hexagon))

; Selektiert eine zufaellige Element aus der Liste
(define (rand-mem xs) (list-ref xs (random (length xs))))
; Selektiert eine zufaellige Element aus einer Paare
(define (rand-mem-pair p) 
 (let ([rand (random)])
   (if (< rand 0.5) (car p) (cdr p))))

; Aufgabe 1.2.1
; Gibt eine Liste von rezessive Merkmale eines
; dominanten Merkmals zurueck
; (rezessiv-merkmale
;   typ - ein von '(Musterungen Fluegelfarben Fluegelformen Fuehlerformen)
;   m   - das dominante Merkmal
; )
(define (rezessiv-merkmale typ m)
  (let ([mem (member m typ)])
    (if (list? mem) mem '())))
;(~a (rezessiv-merkmale Musterungen 'star))
;(~a (rezessiv-merkmale Musterungen 'dots))
;(~a (rezessiv-merkmale Fluegelfarben 'green))
;(~a (rezessiv-merkmale Fluegelfarben 'red))

; Aufgabe 1.2.2
; Zwei Merkmal auf Dominanz vergleichen
; Gibt #t zurueck wenn m1 dominiert m2.
; Wenn zwei Merkmale nicht von demselben Typ sind
; wird #f zurueckgegeben
; Falls m1 und m2 gleich sind dann #t
(define (member? x xs)
  (if (or (null? xs)
          (empty? xs)) #f
      (or (equal? x (car xs)) (member? x (cdr xs)))))
(define (cmp-merkmal typ m1 m2)
  (member? m2 (rezessiv-merkmale typ m1)))
; Testdaten
; (~a (cmp-merkmal Musterungen 'star 'stripes))
; (~a (cmp-merkmal Musterungen 'stripes 'dots))

; Aufgabe 1.2.3
; Einen Schmetterling generieren mit dominanten Merkmalen
; die rezessiven Merkmalen werden zufaellig generiert.
; Gibt einen Schmetterling-Verbund zurueck.
; (gen-butterfly
;   fluegelfarbe - dominante Fluegelfarbe (symbol)
;   musterung - dominante Musterung (symbol)
;   fuehlerform - dominante Fuehlerformen  (symbol)
;   fluegelform - dominante Fluegelformen (symbol)
; )
(define (gen-butterfly fluegelfarbe musterung fuehlerform fluegelform)
  (make-butterfly
   (cons fluegelfarbe (rand-mem (rezessiv-merkmale Fluegelfarben fluegelfarbe)))
   (cons musterung (rand-mem (rezessiv-merkmale Musterungen musterung)))
   (cons fuehlerform (rand-mem (rezessiv-merkmale Fuehlerformen fuehlerform)))
   (cons fluegelform (rand-mem (rezessiv-merkmale Fluegelformen fluegelform)))))
(define sample-butterfly (gen-butterfly 'red 'star 'curly 'hexagon))
; (~a (butterfly-fluegelfarbe sample-butterfly))
; (~a (butterfly-musterung sample-butterfly))
; (~a (butterfly-fuehlerform sample-butterfly))
; (~a (butterfly-fluegelform sample-butterfly))

; Aufgabe 1.2.4
; Gibt eine Liste zurueck mit allen Merkmalen (sichtbar und unsichtbar)
; Jedes Element ist eine Paare (sichtbar-merkmal . unsichtbar-merkmal)
(define (get-merkmale b)
  (list (butterfly-musterung b) (butterfly-fluegelfarbe b) (butterfly-fuehlerform b) (butterfly-fluegelform b)))
;(display (get-merkmale sample-butterfly))

; Aufgabe 1.2.5
; Anzeige eines Schmetterlings als Bild
(define (butterfly-anzeigen b)
  (show-butterfly (car (butterfly-fluegelfarbe b)) (car (butterfly-musterung b)) (car (butterfly-fuehlerform b)) (car (butterfly-fluegelform b))))
; Testdaten
; (car (butterfly-fluegelfarbe sample-butterfly))
; (butterfly-anzeigen sample-butterfly)

; Aufgabe 1.2.6
; Kinder aus den Schmetterling-Eltern generieren
; und gibt eine liste von Kindern zurueck
; (gen-kinder
;    n - Anzahl von Kinder (number)
;    mutter - Mutter der Kinder (butterfly)
;    vater - Vater der Kinder (butterfly)
; )

; Hilffunktion um zwei Merkmal nach Dominanz zu sortieren
; z.B. (sort-merkmal (cons 'red 'blue) Fluegelfarben) -> '(blue . red)
(define (sort-merkmal m1 m2 typ)
  (if (cmp-merkmal typ m1 m2) (cons m1 m2) (cons m2 m1)))
(define (gen-kinder n mutter vater)
  (if (<= n 0) '()
    (cons
     (let ([fluegelfarbe-mutter (rand-mem-pair (butterfly-fluegelfarbe mutter))]
           [musterung-mutter    (rand-mem-pair (butterfly-musterung mutter))]
           [fuehlerform-mutter  (rand-mem-pair (butterfly-fuehlerform mutter))]
           [fluegelform-mutter  (rand-mem-pair (butterfly-fluegelform mutter))]
           [fluegelfarbe-vater  (rand-mem-pair (butterfly-fluegelfarbe vater))]
           [musterung-vater     (rand-mem-pair (butterfly-musterung vater))]
           [fuehlerform-vater   (rand-mem-pair (butterfly-fuehlerform vater))]
           [fluegelform-vater   (rand-mem-pair (butterfly-fluegelform vater))])
       (make-butterfly
         (sort-merkmal fluegelfarbe-mutter fluegelfarbe-vater Fluegelfarben)
         (sort-merkmal musterung-mutter musterung-vater Musterungen)
         (sort-merkmal fuehlerform-mutter fuehlerform-vater Fuehlerformen)
         (sort-merkmal fluegelform-mutter fluegelform-vater Fluegelformen)))
     (gen-kinder (- n 1) vater mutter))))
; Testdaten
(define sample-mutter (gen-butterfly 'red 'star 'curly 'hexagon))
(define sample-vater (gen-butterfly 'green 'dots 'curved 'rhomb))
(define sample-kinder (gen-kinder 3 sample-mutter sample-vater))
; Aufgabe 1.2.7
(define (butterflies-anzeigen xs)
   (if (empty? xs) '()
       (cons (butterfly-anzeigen (car xs)) (butterflies-anzeigen (cdr xs)))))
; Testdaten
(get-merkmale sample-mutter)
(get-merkmale sample-vater)
(butterfly-anzeigen sample-mutter)
(butterfly-anzeigen sample-vater)
(butterflies-anzeigen sample-kinder)
