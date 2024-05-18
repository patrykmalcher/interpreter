# Fox (język programowania)

## Tabelka

Na 15 punktów
- 01 (trzy typy)
- 02 (literały, arytmetyka, porównania)
- 03 (zmienne, przypisanie)
- 04 (print)
- 05 (while, if)
- 06 (funkcje lub procedury, rekurencja)
- 07 (przez zmienną / przez wartość / in/out)

Na 20 punktów
- 09 (przesłanianie i statyczne wiązanie)
- 10 (obsługa błędów wykonania)
- 11 (funkcje zwracające wartość)

Na 26 punktów
- 12 (4) (statyczne typowanie)
- 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)

Razem: 26

## Opis

Fox jest statycznie typowanym językiem imperatywnym. Znacząca część języka będzie zapożyczona z języka Latte. Język ten działa standardowo ze standardową składnią.

## Program

Program w języku Fox jest listą Instrukcji.

```ebnf
entrypoints Program ;

Prog. Program ::= [Stmt] ;
```

## Instrukcje

Większość dziala standardowo - jak w Latte, czyli składniowo jak C/Java.

Blok:

```ebnf
Bl. Block ::= "{" [Stmt] "}" ;

separator  Stmt "" ;

BlStmt. Stmt ::= Block ;
```

Deklaracja - konieczna jest inicjalizacja:

```ebnf
Decl.      Stmt ::= Type [Item] ";" ;

Init.      Item ::= Ident "=" Expr ;

separator nonempty Item "," ;
```

Przypisanie:

```ebnf
Ass. Stmt ::= Ident "=" Expr ";" ;
```

Return - musi zwracać typ:

```ebnf
Ret. Stmt ::= "return" Expr ";" ;
```

If + else:

```ebnf
If. Stmt ::= "if" "(" Expr ")" Stmt  ;

IfElse. Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt  ;
```

While:

```ebnf
While. Stmt ::= "while" "(" Expr ")" Stmt ;
```

Wyrażenie:

```ebnf
SExp.      Stmt ::= Expr  ";" ;
```

Deklaracja funkcji:

```ebnf
FSt. Stmt ::= Fun ;
```

## Funkcje

Funkcja składa się z:
- typu zwracanej wartości
- nazwy
- listy argumentów
- ciała

```ebnf
FunDef. Fun ::= Type Ident "(" [Arg] ")" Block ;

ArgVar. Arg ::= "var" Type Ident ;

ArgVal. Arg ::= Type Ident ;

separator  Arg "," ;
```

### Paramatry funkcji

Możliwe jest przekazanie parametru przez wartość lub przez zmienną. W przypadku przekazania parametru przez zmienną, przy definicji parametru konieczne jest słowo kluczowe `var`.
```C
int przezZmienna(var int x) {
    ...
    return 0;
}
```
Wewnątrz funkcji parametry formalne zachowują się jak zmienne lokalne. Funkcje są rekurencyjne, dowolnie zagnieżdżone i zachowują się zgodnie z zachowaniem poprawności statycznego wiązania identyfikatorów. Każda funkcja musi zakończyć się operacją `return` z odpowiednim typem zwracajnej wartości.

### Ciało funkcji

Ciało funkcji składa się z z bloku instrukcji.


## Typy

Trzy podstawowe: int, string, boolean. W szczególności brak typu void.

```ebnf
Int.  Type ::= "int" ;

Str.  Type ::= "string" ;

Bool. Type ::= "boolean" ;
```

## Wyrażenia

Podzbiór zbioru wyrażeń dostępnych w Javie - Taki jak w Latte. Wyrażenia logiczne zwracają typ boolean i są obliczane leniwie. Arytmetyka zupełnie standardowa.

```ebnf
EVar.      Expr6 ::= Ident ;

ELitInt.   Expr6 ::= Integer ;

ELitTrue.  Expr6 ::= "true" ;

ELitFalse. Expr6 ::= "false" ;

EApp.      Expr6 ::= Ident "(" [Expr] ")" ;

EString.   Expr6 ::= String ;

Neg.       Expr5 ::= "-" Expr6 ;

Not.       Expr5 ::= "!" Expr6 ;

EMul.      Expr4 ::= Expr4 MulOp Expr5 ;

EAdd.      Expr3 ::= Expr3 AddOp Expr4 ;

ERel.      Expr2 ::= Expr2 RelOp Expr3 ;

EAnd.      Expr1 ::= Expr2 "&&" Expr1 ;

EOr.       Expr ::= Expr1 "||" Expr ;

coercions  Expr 6 ;

separator  Expr "," ;
```

## Operatory

j.w.

```ebnf
Plus.      AddOp ::= "+" ;

Minus.     AddOp ::= "-" ;

Times.     MulOp ::= "*" ;

Div.       MulOp ::= "/" ;

Mod.       MulOp ::= "%" ;

LTH.       RelOp ::= "<" ;

LE.        RelOp ::= "<=" ;

GTH.       RelOp ::= ">" ;

GE.        RelOp ::= ">=" ;

EQU.       RelOp ::= "==" ;

NE.        RelOp ::= "!=" ;
```

## Komentarze

```ebnf
comment    "//" ;
```

### Predefiniowane funkcje

```C
int printInt(int)
int printString(string)
int printBoolean(boolean)
int error()
```

Funkcja error wypisuje error i kończy wykonywanie programu. Ze względu na brak typu void, funkcję zwracają wartość 0.

### Konflikty

Gramatyka pozbawiona konfliktów poza standardowym problemem "dangling else", który już występuje w Latte. Zgodnie z dokumentacją BNFC: In case of a shift/reduce conflict, the parser always shifts. 

### Przykładowe programy

```C
// Trzy typy, zmienne, deklaracja.
int i1 = 0, i2 = 0, i3 = 0;
string s1 = "s1", s2 = "s2";
boolean b1 = false;

// Przypisanie, arytmetyka, literały, porównania.
i1 = i2 + i3 * 2;
b3 = i1 < i2;

// Print
printInt(i1);
printBoolean(b3);
```

```C
int i = 0;

// while, if
while (i < 10){
    if (i % 2 == 0) printInt(i) ;
    i = i + 1 ;
}
printInt(i) ;
```

```C
// Funkcje, rekurencja

// iteracyjnie
int fact (int n) {
  int i = 1, r = 1 ;

  while (i < n+1) {
    r = r * i ;
    i = i + 1 ;
  }
  return r ;
}

// rekurencyjnie
int factr (int n) {
  if (n < 2)
    return 1 ;
  else
    return (n * factr(n-1)) ;
}

printInt(fact(7)) ;
printInt(factr(7)) ;
```

```C
// Parametr przez zmienna / wartosc
// Funkcje przyjmujące, zwracające wartość

int przezZmienna(var int zmienna) {
    zmienna = zmienna + 1 ;
    return zmienna;
}

int przezWartosc(int zmienna) {
    zmienna = zmienna + 1 ;
    return zmienna;
}

int x = 0, y = 0;

int skip = przezZmienna(x) + przezWartosc(y);
```

```C
// Zagnieżdżone funkcje, statyczne wiązanie

int zewnetrzna() {
    int x = 500;

    int zmglob() {
        x = x + 1;
        printInt(x); // zewnętrzny x: 501
        return x;
    }

    int zmlok() {
        int x = 10;
        zmglob();
        printInt(x); // lokalny x: 10
        return x;
    }

    int skip1 = zmlok();
    printInt(x); // zewnętrzny x: 501
    return x;
}

int skip = zewnetrzna();
```