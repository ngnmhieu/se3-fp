#lang swindle
(require swindle/setf
         swindle/misc )

; Dokumentation für swindle findet man hier: http://barzilay.org/Swindle

; Aufgabe 1.1

(defclass Literatur ()
  (id :accessor id
      :initarg :id
      :initvalue 0
      :documentation "Eindeutiger Schlüssel eines Literaturs")
  (titel :accessor titel
         :initarg :titel
         :initvalue ""
         :documentation "Titel des Literaturs")
  (jahr :accessor jahr
        :initarg :jahr
        :initvalue 1900
        :documentation "Erscheinungsjahr")
  (autor :accessor autor 
           :initarg :autor
           :initvalue ""
           :documentation "Namen der Authoren bzw. Authorinnen"))

(defclass Buch (Literatur)
  (verlag :accessor verlag
          :initarg :verlag
          :initvalue ""
          :documentation "Name des Verlags")
  (verlag-ort :accessor verlag-ort
              :initarg :verlag-ort
              :initvalue ""
              :documentation "Ort des Verlags")
  (reihe :accessor reihe
         :initarg :reihe
         :initvalue ""
         :documentation "Die Reihe")
  (serien-nr :accessor serien-nr
             :initarg :serien-nr
             :initvalue 0
             :documentation "Die Serien Nummer")
  :printer #t)

(defclass Sammelband (Buch)
  (herausgeber :accessor herausgeber
               :initarg :herausgeber
               :initvalue ""
               :documentation "Name des Herausgebers")
  (seiten :accessor seiten
          :initarg :seiten
          :initvalue 0
          :documentation "Seitenangaben zum Artikel im Buch"))

(defclass Zeitschriftenartikel (Literatur)
  (band-name :accessor band-name
             :initarg :band-name
             :initvalue ""
             :documentation "Name der Zeitschrift")
  (band-nr :accessor band-nr
           :initarg :band-nr
           :initvalue 0
           :documentation "Band Nummer")
  (heft-nr :accessor heft-nr
           :initarg :heft-nr
           :initvalue 0
           :documentation "Heft Nummer")
  (monat :accessor monat
         :initarg :monat
         :initvalue 1
         :documentation "Erscheinungsmonat"))

(define ein-buch (make Buch
      :id 1
      :titel "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
      :jahr 1790
      :autor "Nessie"
      :verlag "Minority-Verlag"
      :verlag-ort "Inverness"
      :reihe "Die besondere Biographie"
      :serien-nr 1))

(define ein-sammelband (make Sammelband
      :id 2
      :titel "Mostly harmless - some observations concerning the third planet of the solar system"
      :jahr 1979
      :autor "Perfect, F"
      :verlag "Galactic Press"
      :verlag-ort "Vega-System, 3rd planet"
      :reihe "Travel in Style"
      :serien-nr 5
      :herausgeber "Adams, D." 
      :seiten 500))

(define ein-zeitschirftenartikel (make Zeitschriftenartikel
      :id 3
      :titel "Zeitmaschinen leicht gemacht"
      :jahr 3200
      :autor "Wells, H. G."
      :band-name "Heimwerkerpraxis für Anfänger"
      :band-nr 3
      :heft-nr 500))

; Aufgabe 1.2

(defgeneric cite ((lit Literatur)))

(defmethod cite ((b Buch))
  (string-append (autor b) " (" (number->string (jahr b)) "). "
                 (titel b) ", Band " (number->string (serien-nr b))
                 " der Reihe: " (reihe b) ". " (verlag b) ", " (verlag-ort b) "."))

(defmethod cite ((s Sammelband))
  (string-append (autor s) " (" (number->string (jahr s)) "). "
                 (titel s) ". In " (herausgeber s) ", Band " (number->string (serien-nr s))
                 " der Reihe: " (reihe s) ". " (verlag s) ", " (verlag-ort s) ", p. "
                 (number->string (seiten s)) "."))

(defmethod cite ((z Zeitschriftenartikel))
  (string-append (autor z) " (" (number->string (jahr z)) "). "
                 (titel z) ". " (band-name z) ", " (number->string (heft-nr z))
                 "(" (number->string (band-nr z)) ")."))

(display (cite ein-buch))
(display "\n\n")
(display (cite ein-sammelband))
(display "\n\n")
(display (cite ein-zeitschirftenartikel))
(display "\n\n")

; Aufgabe 1.3
; Ergänzungsmethode werden eingesetzt, wenn man eine Methode in der Oberklasse wiederverwenden möchte.
; Im Unterschied zu dem 'super'-Aufruf wie in Java, indem man die Methode der Oberklasse überschreibt
; und 'super' nutzt, um diese aufzurufen, definiert man zusätzliche Methode, die davor oder danach
; aufgerufen werden. Vorteil davon wäre, dass es keinen Flüchtigkeitsfehler gibt, wie z.B. dass man
; vergisst super aufzurufen - es wird sichergestellt dass alle Ergänzungsmethode aufgerufen werden.

; Um Ergänzungsmethode in unseren Aufgaben verwenden zu können, lässt sich z.B. die Methode cite so
; ändern, dass es einmal für die Klasse Literatur definiert ist, die allgemeinen Angaben ausgibt, und
; Ergänzungsmethode für die Unterklassen für die Ausgabe von spezifische Informationen.


; Aufgabe 2.1 und 2.2

(defclass Speichermedium ()
  (typ       :accessor typ
             :initarg :typ
             :initvalue ""
             :documentation "Speichertyp eines Speichermediums")
  (maxLesen  :accessor maxLesen
             :initarg :maxLesen
             :initvalue 0
             :documentation "Maximale Lesegeschwindkeit eines Speichermediums in MByte/s")
  (kapazität :accessor kapazität
             :initarg :kapazität
             :initvalue 0
             :documentation "Kapazität eines Speichermediums in GB")
  (lebensd   :accessor lebensdauer
             :initarg :lebensdauer
             :initvalue 0
             :documentation "Lebensdauer eines Speichermediums in Monaten")
  (mobilität :accessor mobilität
             :initarg :mobilität
             :initvalue ""
             :documentation "Mobilität eines Speichermediums")
  (art       :accessor art
             :initarg :art
             :initvalue ""
             :documentation "Art eines Speichermediums, bsp.: SSD, HDD, CD"))

; Klassen die von Speichermdium erben
(defclass magnetisch (Speichermedium)
   (typ :initvalue "magnetisch"))

(defclass herausnehmbaresOptisches (Speichermedium)
  (typ :initvalue "optisch")
  )

(defclass Halbleiter (Speichermedium)
  (typ :initvalue "Halbleiter"))

; Klassen die von bestimmtem Typ Speichermedium erben
(defclass festverbauteHDD (magnetisch)
  (mobilität :initvalue "fest verbaut")
  (art :initvalue "HDD"))

(defclass herausnehmbareDiskette (magnetisch)
  (mobilität :initvalue "herausnehmbar")
  (art :initvalue "Diskette"))

(defclass festverbauteSSD (Halbleiter)
  (mobilität :initvalue "fest verbaut")
  (art :initvalue "SSD"))

(defclass festverbauterRAM (Halbleiter)
  (mobilität :initvalue "fest verbaut")
  (art :initvalue "RAM"))

(defclass herausnehmbareSSD (Halbleiter)
  (mobilität :initvalue "herausnehmbar")
  (art :initvalue "SSD"))

(defclass herausnehmbarerUSBSTICK (Halbleiter)
  (mobilität :initvalue "herausnehmbar")
  (art :initvalue "USB-Stick"))

; Klassen gemischter Speichermedien

(defclass MagnetoOpticalDisc (magnetisch herausnehmbaresOptisches)
  (mobilität :initvalue "herausnehmbar"))

(defclass SSHD (festverbauteHDD festverbauteSSD))

(defclass Bankkarte (Halbleiter)
  (mobilität :initvalue "herausnehmbar"))

; generische Klassen 
; 1. Abfrage des Speichertyps auf dem die Daten gesichert werden
(defgeneric show-typ ((s Speichermedium)))
; 2. Abfrage der Maximallesegeschwindigkeit
(defgeneric show-maxLesen ((s Speichermedium)))
; 3. Abfrage der Kapazität (Speicherplatz)
(defgeneric show-kapazität ((s Speichermedium)))
; 4. Abfrage der durchschnittlichen Lebensdauer
(defgeneric show-lebensd ((s Speichermedium)))
; 5. Abfrage der Mobilität.
(defgeneric show-art ((s Speichermedium)))

; Aufgabe 2.3
; Abfrage der Kapazität
(defmethod show-kapazität ((fvHDD festverbauteHDD))
  (string-append "festverbaute HDD: " (number->string (kapazität fvHDD)) " GB"))

(defmethod show-kapazität ((fvSSD festverbauteSSD))
  (string-append "festverbaute SSD: " (number->string (kapazität fvSSD)) " GB"))

(defmethod show-kapazität ((hSSD herausnehmbareSSD))
  (string-append "herausnehmbare SSD: " (number->string (kapazität hSSD)) " GB"))

(defmethod show-kapazität ((hDiskette herausnehmbareDiskette))
  (string-append "herausnehmbare Diskette: " (number->string (kapazität hDiskette)) " GB"))

(defmethod show-kapazität ((fvRAM festverbauterRAM))
  (string-append "festverbauter RAM: " (number->string (kapazität fvRAM)) " GB"))

(defmethod show-kapazität ((hUSB herausnehmbarerUSBSTICK))
  (string-append "herausnehmbarer USB: " (number->string (kapazität hUSB)) " GB"))

(defmethod show-kapazität ((MOD MagnetoOpticalDisc))
  (string-append "MagnetoOpticalDisc: " (number->string (kapazität MOD)) " GB"))

(defmethod show-kapazität ((SD SSHD))
  (string-append "SSHD: " (number->string (kapazität SD)) " GB"))

(defmethod show-kapazität ((BK Bankkarte))
  (string-append "Bankkarte: " (number->string (kapazität BK)) " GB"))

; definierte Speichermedien
(define HDD (make festverbauteHDD
      :maxLesen 200
      :kapazität 500
      :lebensd 5))

(define SSDverbaut (make festverbauteSSD
      :maxLesen 500
      :kapazität 250
      :lebensd 10))

(define SSDentfernbar (make herausnehmbareSSD
      :maxLesen 400
      :kapazität 500
      :lebensd 10))

(define Diskette (make herausnehmbareDiskette
      :maxLesen 120
      :kapazität 50
      :lebensd 3))

(define RAM (make festverbauterRAM
      :maxLesen 120
      :kapazität 2
      :lebensd 8))

(define USBSTICK (make herausnehmbarerUSBSTICK
      :maxLesen 300
      :kapazität 8
      :lebensd 4))

(define MagnetoOpticalDisc1 (make MagnetoOpticalDisc
      :maxLesen 200
      :kapazität 125
      :lebensd 3))

(define SSHD1 (make SSHD
      :maxLesen 200
      :kapazität 1000
      :lebensd 2))

(define Karte (make Bankkarte
      :maxLesen 100
      :kapazität 0.1
      :lebensd 2))

; Beispielaufrufe
(display (show-kapazität HDD))
(display "\n\n")
(display (show-kapazität SSDverbaut))
(display "\n\n")
(display (show-kapazität SSDentfernbar))
(display "\n\n")
(display (show-kapazität Diskette))
(display "\n\n")
(display (show-kapazität RAM))
(display "\n\n")
(display (show-kapazität USBSTICK))
(display "\n\n")
(display (show-kapazität MagnetoOpticalDisc1))
(display "\n\n")
(display (show-kapazität SSHD1))
(display "\n\n")
(display (show-kapazität Karte))
