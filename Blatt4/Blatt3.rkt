#lang racket
(require racket/gui/base)
(require se3-bib/flaggen-module)

(provide codierung)

; Aufgabe 1.1
(define tafel
  '(("A" "Alpha")
    ("B" "Bravo")
    ("C" "Charlie")
    ("D" "Delta")
    ("E" "Echo")
    ("F" "Foxtrot")
    ("G" "Golf")
    ("H" "Hotel")
    ("I" "India")
    ("J" "Juliet")
    ("K" "Kilo")
    ("L" "Lima")
    ("M" "Mike")
    ("N" "November")
    ("O" "Oscar")
    ("P" "Papa")
    ("Q" "Quebec")
    ("R" "Romeo")
    ("S" "Sierra")
    ("T" "Tango")
    ("U" "Uniform")
    ("V" "Viktor")
    ("W" "Whiskey")
    ("X" "X-ray")
    ("Y" "Yankee")
    ("Z" "Zulu")
    ("0" "Nadazero")
    ("1" "Unaone")
    ("2" "Duotwo")
    ("3" "Terrathree")
    ("4" "Carrefour")
    ("5" "Pentafive")
    ("6" "Soxisix")
    ("7" "Setteseven")
    ("8" "Oktoeight")
    ("9" "Novonine")
    ("," "Decimal")
    ("." "Stop")))

; Aufgabe 1.2
(define (buchstabe->schluessel bs)
  (if (not (char? bs)) #f
      (let ([result (assoc (string bs) tafel)])
      (if (not result) #f (car (cdr result))))))

; Aufgabe 1.3
(define (codierung text)
  (letrec ([_codierung
            (lambda (textchars)
              (if (empty? textchars) '()
                  (cons (buchstabe->schluessel (car textchars)) (_codierung (cdr textchars)))))])
(_codierung (string->list text))))

; Aufgabe 2.1
; Assoziationslisten, sind Listen aus Paaren. Sie können leicht mit assoc, mit Angabe eines Schlüssels, durchsucht werden.
(define flaggenalfabet
  '((#\A A)
    (#\B B)
    (#\C C)
    (#\D D)
    (#\E E)
    (#\F F)
    (#\G G)
    (#\H H)
    (#\I I)
    (#\J J)
    (#\K K)
    (#\L L)
    (#\M M)
    (#\N N)
    (#\O O)
    (#\P P)
    (#\Q Q)
    (#\R R)
    (#\S S)
    (#\T T)
    (#\U U)
    (#\V V)
    (#\W W)
    (#\X X)
    (#\Y Y)
    (#\Z Z)
    (#\0 Z0)
    (#\1 Z1)
    (#\2 Z2)
    (#\3 Z3)
    (#\4 Z4)
    (#\5 Z5)
    (#\6 Z6)
    (#\7 Z7)
    (#\8 Z8)
    (#\9 Z9)))

; Aufgabe 2.2
; Funktion, die einen Buchstaben (Typ char) mittels der Flaggentafel auf das Bild der Flagge abbildet:
(define (bs->flagge bs)
  (eval                             ; Wertet zweites Element aus Paar aus und liefert die Flagge als Wert aus der importierten se3-bib
   (cadr                            ; cadr->(car (cdr y )) gibt das 2. Element.
    (assoc bs flaggenalfabet))))    ; assoc durchsucht Liste von Listen (flaggenalfabet), nach einem Paar, wessen erstes Element gleich dem schlüssel ist (bs) 

; Aufagbe 2.3
; Funktion die nach Außen hin aus einem String eine Liste aus Flaggen erstellt, aber eigentlich aus dem übergebenen String eine Liste aus Chars macht
; und aus dieser Liste, mit Hilfe einer anderen Fkt. "bsChar->flaggen" eine Flaggenliste erstellt.
(define (bsString->flaggen bsString)
  (bsChar->flaggen (string->list bsString)))

; Hilfsfunktion, die aus einer gegebenen Liste aus chars eine Liste aus Flaggen erstellt.
(define (bsChar->flaggen bsChar)
  (if (empty? bsChar) #f                       
      (cons(bs->flagge (car bsChar))            ; sonst: erstelle eine Flaggen-Liste mit dem ersten Element der Char-Liste...   
           (bsChar->flaggen (cdr bsChar)))))    ; ...und rufe dich erneut auf um die weiteren Elemente der Char-Liste der Flaggen-Liste hinzuzufügen.    


; Aufgabe 3.1
; Returns
(define morse-codes
  (letrec ([morse (lambda (charlist)
                    (if (empty? charlist) '()
                        (cons (list (car charlist) (lambda () (play-sound (string-append "./morse-wav/Morse-" (car charlist) ".wav") #f)))
                              (morse (cdr charlist)))))])
    (morse '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))))

; Aufgabe 3.2
(define (buchstabe->morse bs)
  (if (not (char? bs)) #f
      (let ([result (assoc (string bs) morse-codes)])
      (if (not result) #f (car (cdr result))))))

; Aufgabe 3.3
(define (morse-codierung text)
  (letrec ([_codierung
            (lambda (textchars)
              (if (empty? textchars) '()
                  (cons ((buchstabe->morse (car textchars))) (_codierung (cdr textchars)))))])
(_codierung (string->list text))))
