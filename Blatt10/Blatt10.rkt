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

; 1.1.1 xy->index wandelt ein 2D-Index in 1D-Index 
(define (xy->index x y) (+ (* y 9) x))
; Testdaten
(~a "xy->index 3 1 -> 12: " (xy->index 3 1))

; 1.1.2
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

; 1.1.3
(define (spiel->eintraege spiel indizes)
  (map (curry vector-ref spiel) indizes))

; Testdaten
(~a "(spiel->eintraege spiel (quadrant->indizes 8)): " (spiel->eintraege spiel (quadrant->indizes 8)))

; 1.1.4
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

; 1.2.1
(define (markiere-auschluss original-spiel n)
  (let* ([spiel (vector-copy original-spiel)]
         [markiere (lambda (indizes)
                     (let (; bestimmt ob die Zahl n in der liste vorkommt
                           [appears-in-indizes? (foldl (lambda (x result) (or result (equal? x n)))
                                               #f (spiel->eintraege spiel indizes))]
                           ; findet alle Indizes deren Inhalt Null sind 
                           [null-indizes (filter (lambda (i) (equal? 0 (vector-ref spiel i))) indizes)])
                       ; falls n in der Liste vorkommt, setze alle Null in 'X
                       (if appears-in-indizes?
                         (for/list ([i null-indizes])
                           (vector-set! spiel i 'X))
                         (void))))])
    (for ([i (range 9)])
      (markiere (zeile->indizes i))
      (markiere (spalte->indizes i))
      (markiere (quadrant->indizes i)))
    spiel))

; Testdaten
(display "(markiere-auschluss spiel 2)\n")
(markiere-auschluss spiel 2)
(display "(markiere-auschluss spiel 5)\n")
(markiere-auschluss spiel 5)

; 1.2.2 
(define (eindeutige-positionen original-spiel n)
  (let* ([spiel (markiere-auschluss original-spiel n)]
         ; eindeutig-position-in (einer Zeile / Spalte / Quadrant)
         ; gibt entweder eine positive Zahl oder -1 zurück,
         ; falls keine Position gefunden werden kann.
         [eindeutige-position-in (lambda (indizes)
                                   (let ([num-nulls (count (curry equal? 0) (spiel->eintraege spiel indizes))])
                                     (if (equal? 1 num-nulls)
                                       ; findet die Position der eindeutigen Null
                                       (findf (lambda (i) (equal? 0 (vector-ref spiel i))) indizes)
                                       ; -1 falls es mehr als eine Null gibt
                                       -1)))]
         [eindeutige-positionen-in (lambda (indizes-liste)
                                     (filter (curry <= 0)
                                             (map eindeutige-position-in
                                                  indizes-liste)))])
    (remove-duplicates
      (append 
        (eindeutige-positionen-in (map zeile->indizes (range 9)))
        (eindeutige-positionen-in (map spalte->indizes (range 9)))
        (eindeutige-positionen-in (map quadrant->indizes (range 9)))))))

; Testdaten
(display "(eindeutige-positionen spiel 2)\n")
(eindeutige-positionen spiel 2)
(display "(eindeutige-positionen spiel 5)\n")
(eindeutige-positionen spiel 5)

; 1.2.3
(define (loese-spiel original-spiel)
  (letrec ([loese-recursive
             (lambda (spiel)
               (let ([setze (lambda (i s)
                              (let ([positionen (eindeutige-positionen s i)])
                                (for ([pos positionen]) (vector-set! s pos i))
                                s))])
                 (if (spiel-geloest? spiel) spiel
                   (loese-recursive (foldl setze spiel (range 1 10))))))])
    (loese-recursive (vector-copy original-spiel))))

; Testdaten
(define sample-spiel #(2 3 0 0 0 0 0 7 0
                       0 0 0 6 0 0 0 9 0
                       0 0 9 2 0 0 5 0 0
                       3 0 0 8 4 0 0 0 0
                       0 8 0 0 1 0 0 3 0
                       0 0 0 0 2 7 0 0 6
                       0 0 1 0 0 9 4 0 0
                       0 2 3 0 0 8 0 0 0
                       0 7 0 0 0 0 0 6 3))

(define sample-spiel-loesung #(2 3 8 5 9 4 6 7 1
                               4 5 7 6 8 1 3 9 2
                               6 1 9 2 7 3 5 4 8
                               3 9 2 8 4 6 1 5 7
                               7 8 6 9 1 5 2 3 4
                               1 4 5 3 2 7 9 8 6
                               8 6 1 7 3 9 4 2 5
                               5 2 3 4 6 8 7 1 9
                               9 7 4 1 5 2 8 6 3))
; (display "(loese-spiel spiel): \n")
; (loese-spiel spiel)

(display "(loese-spiel sample-spiel): \n")
(eindeutige-positionen sample-spiel 9)
