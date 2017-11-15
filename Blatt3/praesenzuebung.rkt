#lang racket

; Aufgabe 3
(define (merge listA listB)
  (letrec ([_merge
             (lambda (mergedList listA listB)
               (cond
                 [(and (empty? listA) (empty? listB)) (reverse mergedList)]
                 [(empty? listA) (_merge (cons (car listB) mergedList) listA (cdr listB))]
                 [(empty? listB) (_merge (cons (car listA) mergedList) (cdr listA) listB)]
                 [else (let ([a (car listA)]
                             [b (car listB)])
                         (if (<= a b)
                           (_merge (cons a mergedList) (cdr listA) listB)
                           (_merge (cons b mergedList) listA (cdr listB))))]))])
    (_merge '() listA listB)))

; Alternative Loesung
(define (merge a b)
  (cond ((empty? a) b)
        ((empty? b) a)
        ((< (car a) (car b)) (cons (car a) (merge (cdr a) b)))
        (else (cons (car b) (merge a (cdr b))))))

; mit trace-define kann man rekursive Funktion debuggen

; Aufgabe 4
(define (teile liste)
  (letrec ([_teile (lambda (even odd liste)
                     (if (empty? liste) '((reverse even) (reverse odd))
                       (let ([el (car liste)])
                         (if (even? el)
                           (_teile (cons el even) odd (cdr liste))
                           (_teile even (cons el odd) (cdr liste))))))])
    (_teile '() '() liste)))
