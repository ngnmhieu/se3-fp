#lang racket
(require racket/gui/base)

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