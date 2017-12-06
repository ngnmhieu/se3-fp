#lang racket

(require 2htdp/image)
(require lang/posn)

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
; (take 5 '(1 2 3 4 5 6))
; (take 1 '(1 2))
; (take 2 '())
; (take 10 null)

; Aufgabe 2

; Ein Wagen mit zwei Reifen
(define wagen
  (place-images/align 
    (list (ellipse 90 90 "solid" "black") ; erste Reife
          (ellipse 90 90 "solid" "black") ; zweite Reife
          (rectangle 400 100 "solid" "gray")) ; Wagen
    (list (make-posn 50 80)
          (make-posn 260 80)
          (make-posn 0 20))
    "left" "top"
    (rectangle 400 170 "solid" "white")))

; eine Kerze mit einer bestimmten farbe
(define (kerze farbe)
  (let* ([feuer-width  16]
         [feuer-height 30]
         [feuer-margin 20]
         [feuer (overlay/align "center" "bottom"
                               (ellipse (* feuer-width 0.4) (* feuer-height 0.4) "solid" "brown") ; 1. feuer ellipse
                               (ellipse (* feuer-width 0.7) (* feuer-height 0.7) "solid" "red"  ) ; 2. feuer ellipse
                               (ellipse feuer-width feuer-height "solid" "orange") ; 3. (groesste) feuer-ellipse
                               (rectangle (+ feuer-width feuer-margin) (+ feuer-height feuer-margin) "solid" "white"))]
         [kerze-rumpf (rectangle 30 50 "solid" farbe)])
    (above/align "center" feuer kerze-rumpf)))

; ein Geschenk mit einer bestimmten farbe
(define (geschenk farbe)
  (let* ([geschenk-width  60]
         [geschenk-height 40]
         [geschenk-margin 20])
    (overlay/align "center" "bottom"
                   (overlay/align "center" "center"
                                  (rectangle (* geschenk-width 0.1) geschenk-height "solid" "orange")
                                  (rectangle geschenk-width (* geschenk-height 0.1) "solid" "orange")
                                  (rectangle geschenk-width geschenk-height "solid" farbe))
                   (rectangle (+ geschenk-width geschenk-margin) (+ geschenk-height geschenk-margin) "solid" "white"))))

; gen-baum-von generiert einen Baum mit mehreren Ebenen von eines Gegenstand.
; `n`  ist die Anzahl von Ebenen.
; `ding` ist ein Gegenstand-Generator der eine Farbe aufnimmt
; `farbgen` ist eine Funktion die Farben generieren
(define (gen-baum-von n ding farbgen)
  (cond
    [(< n 1) (error "n muss > 0 sein.")]
    [(= n 1) (gen-zeile-von 1 ding farbgen)]
    [else 
      (above/align "center"
                   (gen-baum-von (- n 1) ding farbgen)
                   (gen-zeile-von n ding farbgen))]))

; gen-zeile-von generiert eine Zeile mit den Objekten neben einander
; `n` ist die Anzahle der Dingen.
; `ding` ist ein Gegenstand-Generator der eine Farbe aufnimmt
; `farbgen` ist eine Funktion die Farben generieren
(define (gen-zeile-von n ding farbgen)
  (cond
    [(< n 1) (error "n muss > 0 sein.")]
    [(= n 1) (ding (farbgen))]
    [else 
      (beside/align "bottom"
                    (ding (farbgen))
                    (gen-zeile-von (- n 1) ding farbgen))]))

; gibt zufaellige farbe zurueck
(define farben #("red" "green" "blue" "purple" "pink" "black"))
(define (farben-gen)
  (vector-ref farben (random (vector-length farben))))

; generiert ein Weihnachten-Bild
(define (christmas)
  (beside/align "bottom"
                (gen-baum-von 4 kerze (lambda () "red"))          ; haufen von rote Kerzen
                (rectangle 30 30 "solid" "white")                 ; spacing
                (above/align "center"                             ; wagen mit geschenke
                             (gen-baum-von 4 geschenk farben-gen)
                             wagen)))
(christmas)
