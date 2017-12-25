## Funktion höherer Ordnung

```
# map jedes Element der Liste mit f -> neue Liste mit gleicher Anzahl von Elementen
# f nimmt ein Argument auf
(map f xs) -> f(xs)

# foldl geht von links nach rechts und wendet f auf jedes einzelne
# Element in der Liste und aggregiert das Ergebnis zu dem Startwert s
# f = (lambda (x acc))
(foldl f s xs) -> x 
# man kann fold sogar mehrere Liste übergeben, dann hat f extra Parameter für
# die aktuellen Elemente dieser Liste
# f = (lambda (x1 x2 x3 acc))
(foldl f s xs1 xs2 xs3) -> x

# filter gibt alle Elemente der Liste zurück, die dazu führt dass f zu #t ausgewertet wird
(filter f xs) -> gefilterte xs

# curry erzeugt eine neue Funktion
# in der ein (oder mehrere) Parameter schon vorbelegt ist.
# z.B. falls f hat 3 Paramter x y und z
((curry f) x) -> (f1 y z)
(((curry f) x) y) -> (f2 z)
((((curry f) x) y) z) -> Ergebnis von (f x y z)

# compose kombiniert zwei Funktionen, sodass
# eine Funktion zuerst aufgerufen wird, dann die andere
# mit dem Ergebnis der ersten Funktion aufgerufen wird.
(compose f1 f2) -> f'(x) = f1(f2(x))
```
