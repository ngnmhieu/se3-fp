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

; Aufgabe 2.1


