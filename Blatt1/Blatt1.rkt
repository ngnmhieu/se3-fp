#lang racket
(require racket/format)

;Aufgabe 1.1

;Liefert den Gradmaßwert=g im Bogenmaß.
(define (gradmass->bogenmass g)
        (* (/ g 180) pi))

;Liefert den Bogenmaßwert=b im Gradmaß.
(define (bogenmass->gradmass b)
        (* (/ b pi) 180))

; Aufgabe 1.2
(define (my-acos x)
  (cond [(=  0 x) (* 0.5 pi)]
        [(= -1 x) pi]
        [else (atan (/ (sqrt (- 1 (expt x 2))) (expt x 2)))]))

; Aufgabe 1.3
(define (nm->km x)
  (* x 1.852))

; Aufgabe 2.1
(define (distanzAB latA longA latB longB)
  (nm->km                     ; seemeilen in km umwandeln
   (* 60 (bogenmass->gradmass ; in Minuten umwandeln
          (my-acos            ; dG
           (+ (* (sin (gradmass->bogenmass latA)) (sin (gradmass->bogenmass latB)))
              (* (cos (gradmass->bogenmass latA)) (cos (gradmass->bogenmass latB)) (cos (- (gradmass->bogenmass longA) (gradmass->bogenmass longB))))))))))

(define Oslo->Hongkong (distanzAB 59.93 10.75 22.20 114.10))
(define SanFrancisco->Honolulu (distanzAB 37.75 -122.45 21.32 -157.83))
(define Osterinsel->Lima (distanzAB -27.10 -109.40 -12.10 -77.05))

(~a "Oslo nach Hongkong: " Oslo->Hongkong)
(~a "San Francisco nach Honolulu: " SanFrancisco->Honolulu)
(~a "Osterinsel nach Lima: " Osterinsel->Lima)


; Aufgabe 2.2


; Aufgabe 2.3