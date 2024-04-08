# Fox (język programowania)

Na potrzeby zadania nazwijmy deklarowany język Fox.
Znacząca część języka będzie zapożyczona z Latte (https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2022/Latte/).
Fox jest statycznie typowanym językiem imperatywnym.

## Program

Program w języku Fox jest listą funkcji, która spełnia poniższe warunki: 
- Funkcje w tej liście muszą mieć unikalne nazwy.
- W programie musi wystąpić funkcja o nazwie main zwracająca int i nie przyjmująca argumentów (od niej zaczyna się wykonanie programu).

## Funkcja

Funkcja składa się z:
- typu zwracanej wartości
- nazwy
- listy argumentów
- ciała

### Paramatry funkcji

Możliwe jest przekazanie parametru przez wartość lub przez zmienną. W przypadku przekazania parametru przez zmienną, przy definicji parametru konieczne jest słowo kluczowe `var`.
```C
int przezZmienna(var int x) {
    ...
    return 0;
}
```
Wewnątrz funkcji parametry formalne zachowują się jak zmienne lokalne.

### Ciało funkcji

Ciało funkcji składa się z z bloku instrukcji.


### Instrukcje

- blok
- pusta
- if
- while
- return
- przypisanie
- deklaracja

Instrukcje te są analogiczne do C/Javy.
Deklaracje zmiennych mogą występować w dowolnym miejscu bloku, jednak każda zmienna musi być zadeklarowana przed użyciem. Jeśli zmienna nie jest jawnie inicjalizowana w momencie deklaracji, jest inicjalizowana wartością domyślną (0 dla int, "" dla string, false dla bool). Zmienne zadeklarowane w bloku nie są widoczne poza nim i przesłaniają zmienne o tej samej nazwie spoza bloku. W obrębie bloku zmienne muszą mieć unikalne nazwy. Dodatkowo instrukcją może być definicja funkcji, która wprowadzą nową funkcję widoczną w obecnym zakresie. Funkcje te zachowują się zgodnie z zachowaniem poprawności statycznego wiązania identyfikatorów.

### Typy

Trzy podstawowe: int, string, boolean. W szczególności brak typu void.

### Wyrażenia

Podzbiór zbioru wyrażeń dostępnych w Javie - Taki jak w Latte. Wyrażenia logiczne zwracają typ boolean i są obliczane leniwie.

### Predefiniowane funkcje

```C
void printInt(int)
void printString(string)
void error()
```

Funkcja error wypisuje error i kończy wykonywanie programu.

### Konflikty








    