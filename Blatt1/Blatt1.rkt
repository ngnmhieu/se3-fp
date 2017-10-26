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
  (cond [(not (<= -1 x 1)) (error "Ungueltige Eingabe - sie muss zwischen -1 und 1 sein.")]
        [(=  0 x)   (* 0.5 pi)]
        [(= -1 x)   pi]
        [(< -1 x 0) (+ pi (atan (/ (sqrt(- 1 (expt x 2))) x)))]
        [else       (atan (/ (sqrt(- 1 (expt x 2))) x))]))
; Test daten
(~a "my-acos(cos(0)  ): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 0)))))
(~a "   acos(cos(0)  ): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 0)))))
(~a "my-acos(cos(30) ): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 30)))))
(~a "   acos(cos(30) ): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 30)))))
(~a "my-acos(cos(45) ): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 45)))))
(~a "   acos(cos(45) ): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 45)))))
(~a "my-acos(cos(60) ): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 60)))))
(~a "   acos(cos(60) ): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 60)))))
(~a "my-acos(cos(90) ): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 90)))))
(~a "   acos(cos(90) ): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 90)))))
(~a "my-acos(cos(120)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 120)))))
(~a "   acos(cos(120)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 120)))))
(~a "my-acos(cos(150)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 150)))))
(~a "   acos(cos(150)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 150)))))
(~a "my-acos(cos(180)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 180)))))
(~a "   acos(cos(180)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 180)))))
(~a "my-acos(cos(210)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 180)))))
(~a "   acos(cos(210)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 180)))))
(~a "my-acos(cos(240)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 240)))))
(~a "   acos(cos(240)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 240)))))
(~a "my-acos(cos(270)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 270)))))
(~a "   acos(cos(270)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 270)))))
(~a "my-acos(cos(300)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 300)))))
(~a "   acos(cos(300)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 300)))))
(~a "my-acos(cos(330)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 330)))))
(~a "   acos(cos(330)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 330)))))
(~a "my-acos(cos(360)): " (bogenmass->gradmass (my-acos (cos (gradmass->bogenmass 360)))))
(~a "   acos(cos(360)): " (bogenmass->gradmass (acos (cos (gradmass->bogenmass 360)))))

; Aufgabe 1.3
(define (nm->km x)
  (* x 1.852))

; Aufgabe 2.1
(define (dG latA longA latB longB)
  (my-acos
   (+ (* (sin (gradmass->bogenmass latA)) (sin (gradmass->bogenmass latB)))
      (* (cos (gradmass->bogenmass latA)) (cos (gradmass->bogenmass latB)) (cos (- (gradmass->bogenmass longA) (gradmass->bogenmass longB)))))))
(define (distanzAB latA longA latB longB)
  (nm->km                     ; seemeilen in km umwandeln
   (* 60 (bogenmass->gradmass ; in Minuten umwandeln
          (dG latA longA latB longB)))))

(define Oslo->Hongkong (distanzAB 59.93 10.75 22.20 114.10))
(define SanFrancisco->Honolulu (distanzAB 37.75 -122.45 21.32 -157.83))
(define Osterinsel->Lima (distanzAB -27.10 -109.40 -12.10 -77.05))

(~a "Oslo nach Hongkong: " Oslo->Hongkong)
(~a "San Francisco nach Honolulu: " SanFrancisco->Honolulu)
(~a "Osterinsel nach Lima: " Osterinsel->Lima)

; Aufgabe 2.2
(define (kurs latA longA latB longB)
  (bogenmass->gradmass
   (my-acos
    (/ (- (sin (gradmass->bogenmass latB)) (* (cos (dG latA longA latB longB)) (sin (gradmass->bogenmass latA))))
       (* (cos (gradmass->bogenmass latA)) (sin (dG latA longA latB longB)))))))

(define Kurs-Oslo->Hongkong (kurs 59.93 10.75 22.20 114.10))
(define Kurs-SanFrancisco->Honolulu (kurs 37.75 -122.45 21.32 -157.83))
(define Kurs-Osterinsel->Lima (kurs -27.10 -109.40 -12.10 -77.05))
(~a "Kurs Oslo nach Hongkong: " Kurs-Oslo->Hongkong)
(~a "Kurs San Francisco nach Honolulu: " (- 360 Kurs-SanFrancisco->Honolulu))
(~a "Kurs Osterinsel nach Lima: " Kurs-Osterinsel->Lima)

; Aufgabe 2.3
