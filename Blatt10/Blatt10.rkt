#lang racket

(define spiel #(0 0 0 0 0 9 0 7 0
                0 0 0 0 8 2 0 5 0
                3 2 7 0 0 0 0 4 0
                0 1 6 0 4 0 0 0 0
                0 5 0 0 0 0 3 0 0
                0 0 0 0 9 0 7 0 0
                0 0 0 6 0 0 0 0 5
                8 0 2 0 0 0 0 0 0
                0 0 4 2 0 0 0 0 8))

; Aufgabe 1.1

; 1. xy->index wandelt ein 2D-Index in 1D-Index 
(define (xy->index x y) (+ (* y 9) x))
; Testdaten
(~a "xy->index 3 1 -> 12: " (xy->index 3 1))

; 2.
(define (zeile->indizes z)
  (let ([baseIdx (* z 9)])
    (map (curry + baseIdx) (range 9))))

; Testdaten
(~a "zeile->indizes 0: " (zeile->indizes 0))
(~a "zeile->indizes 3: " (zeile->indizes 3))
(~a "zeile->indizes 8: " (zeile->indizes 8))

(define (spalte->indizes s)
  (map (curry + s)
       (map (curry * 9) (range 9))))

; Testdaten
(~a "zeile->indizes 0: " (spalte->indizes 0))
(~a "zeile->indizes 3: " (spalte->indizes 3))
(~a "zeile->indizes 8: " (spalte->indizes 8))

(define (quadrant->indizes q)
  (let* ([base-quadrant '(0 1 2 9 10 11 18 19 20)]
         [row-offset (* 3 (quotient q 3))]
         [col-offset (* 3 (remainder q 3))]
         [offset (+ col-offset (* row-offset 9))])
    (map (curry + offset) base-quadrant)))

(~a "quadrant->indizes 0: " (quadrant->indizes 0))
(~a "quadrant->indizes 4: " (quadrant->indizes 4))
(~a "quadrant->indizes 8: " (quadrant->indizes 8))

; 3.
(define (spiel->eintraege spiel indizes)
  (map (curry vector-ref spiel) indizes))

; Testdaten
(~a "(spiel->eintraege spiel (quadrant->indizes 8)): " (spiel->eintraege spiel (quadrant->indizes 8)))

; 4.
(define (spiel-konsistent? spiel)
  (let ([valid? (lambda (indizes)
                  ; nach Duplikate prüfen
                  (false? (check-duplicates
                            ; alle Nullen entfernen
                            (filter (lambda (x) (not (equal? x 0)))
                                    (spiel->eintraege spiel indizes)))))])
    ; #t wenn alle #t sind
    (foldl (lambda (x acc) (and x acc)) #t
            (map (lambda (i)
                   (and (valid? (zeile->indizes i))
                        (valid? (spalte->indizes i))
                        (valid? (quadrant->indizes i)))) 
                 (range 9)))))

(define (spiel-geloest? spiel)
  (and (spiel-konsistent? spiel)
       (empty? (filter (curry equal? 0) (vector->list spiel)))))

; Testdaten
(define geloestes-spiel #(4 8 3 9 2 1 6 5 7
                          9 6 7 3 4 5 8 2 1
                          2 5 1 8 7 6 4 9 3
                          5 4 8 1 3 2 9 7 6
                          7 2 9 5 6 4 1 3 8
                          1 3 6 7 9 8 2 4 5
                          3 7 2 6 8 9 5 1 4
                          8 1 4 2 5 3 7 6 9
                          6 9 5 4 1 7 3 8 2))
(~a "(spiel-konsistent? spiel): " (spiel-konsistent? spiel))
(~a "(spiel-geloest? spiel): " (spiel-geloest? geloestes-spiel))

; Aufgabe 1.2
