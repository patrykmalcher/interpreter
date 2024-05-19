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

## Interpreter

### Opis

Interpretowanie programu języka składa się z 3 części.
1. Parsowanie - robione przez BNFC, na tym etapie sprawdzana jest składnia języka.
2. Sprawdzenie typów - wykonywane przez TypeChecker.hs - sprawdza wszystkie błędy, które można wykryć statycznie, takie jak niepoprawność typów, użycie niezadeklarownych zmiennych, funkcje bez returna, czy zła liczba parametrów w wywołaniu.
3. Interpreter - wykonywane przez Interpreter.hs - właściwa interpretacja. Jedyne na tym etapie błędy to błędy wykonania, takie jak np. dzielenie przez 0.

### Typechecker
Poczas sprawdzania typów używam środowiska, który jest krotką `(VEnv, FEnv, Maybe Type, Bool)`. Są to odpowiedno środowisko zmiennych, funkcji, typ funkcji w której aktualnie się znajdujemy, i flaga czy był return.

### Interpreter
W interpreterze środowisko to krotka `(Venv, Fenv)` odpowiednio dająca lokację dla funkcji i zmiennych. Stanem jest krotka `(VStore, FStore, Maybe Value)`, tutaj dodakowo propraguję wartość ostatniego returna.

<br>
Całe technika opiera się na standardowej semantyce programów ze statycznym wiązaniem i przysłanianiem.

### Użycie
Interpreter można zbudować komendą `make`, a następnie używać `./interpreter program` lub `./interpreter`. W 2 przypadku program wczytywany jest ze standardowego wejścia.

### Testowanie
`./test.sh`


## Program

Program w języku Fox jest listą Instrukcji.

```ebnf
entrypoints Program ;

Prog. Program ::= [Stmt];
```

## Instrukcje

Większość dziala standardowo - jak w Latte, czyli składniowo jak C/Java.

Blok:

```ebnf
Bl. Block ::= "{" [Stmt] "}";

separator  Stmt "" ;

BlStmt. Stmt ::= Block;
```

Deklaracja - konieczna jest inicjalizacja:

```ebnf
Decl. Stmt ::= Type Ident "=" Expr ";";
```

Przypisanie:

```ebnf
Ass. Stmt ::= Ident "=" Expr ";";
```

Return - musi zwracać typ:

```ebnf
Ret. Stmt ::= "return" Expr ";";
```

If + else:

```ebnf
If. Stmt ::= "if" "(" Expr ")" Block;

IfElse. Stmt ::= "if" "(" Expr ")" Block "else" Block;
```

While:

```ebnf
While. Stmt ::= "while" "(" Expr ")" Block;
```

Deklaracja funkcji:

```ebnf
FSt. Stmt ::= Fun;
```

## Funkcje

Funkcja składa się z:
- typu zwracanej wartości
- nazwy
- listy argumentów
- ciała

```ebnf
FunDef. Fun ::= Type Ident "(" [Arg] ")" Block;

ArgVar. Arg ::= "var" Type Ident;

ArgVal. Arg ::= Type Ident;

separator Arg "," ;
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
Int. Type ::= "int";

Str. Type ::= "string";

Bool. Type ::= "boolean";
```

## Wyrażenia

Podzbiór zbioru wyrażeń dostępnych w Javie - Taki jak w Latte. Wyrażenia logiczne zwracają typ boolean i są obliczane leniwie. Arytmetyka zupełnie standardowa.

```ebnf
EVar. Expr6 ::= Ident;

ELitInt. Expr6 ::= Integer;

ELitTrue. Expr6 ::= "true";

ELitFalse. Expr6 ::= "false";

EApp. Expr6 ::= Ident "(" [Expr] ")";

EString. Expr6 ::= String;

Neg. Expr5 ::= "-" Expr6;

Not. Expr5 ::= "!" Expr6;

EMul. Expr4 ::= Expr4 MulOp Expr5;

EAdd. Expr3 ::= Expr3 AddOp Expr4;

ERel. Expr2 ::= Expr2 RelOp Expr3;

EAnd. Expr1 ::= Expr2 "&&" Expr1;

EOr. Expr ::= Expr1 "||" Expr;

coercions Expr 6;

separator Expr ",";
```

## Operatory

j.w.

```ebnf
Plus. AddOp ::= "+";

Minus. AddOp ::= "-";

Times. MulOp ::= "*";

Div. MulOp ::= "/";

Mod. MulOp ::= "%";

LTH. RelOp ::= "<";

LE. RelOp ::= "<=";

GTH. RelOp ::= ">";

GE. RelOp ::= ">=";

EQU. RelOp ::= "==";

NE. RelOp ::= "!=";
```

## Komentarze

```ebnf
comment "//";
```

### Predefiniowane funkcje

```ebnf
Print. Stmt ::= "print" Expr ";";
```

### Konflikty

Gramatyka pozbawiona konfliktów.

### Przykładowe programy

Poprawne umieszczone w katalogu `good` or niepoprawne w katologu `bad`.