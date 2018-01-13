#lang swindle
(require swindle/setf
         swindle/misc )

; Dokumentation für swindle findet man hier: http://barzilay.org/Swindle
; Aufgabe 1

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
  (autoren :accessor autoren 
           :initarg :autoren
           :initvalue '()
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
      :autoren (list "Nessie")
      :verlag "Minority-Verlag"
      :verlag-ort "Inverness"
      :reihe "Die besondere Biographie."
      :serien-nr 1))

(define ein-sammelband (make Sammelband
      :id 2
      :titel "Mostly harmless - some observations concerning the third planet of the solar system."
      :jahr 1979
      :autoren (list "Perfect, F")
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
      :autoren (list "Wells, H. G.")
      :band-name "Heimwerkerpraxis für Anfänger"
      :band-nr 3
      :heft-nr 500))

