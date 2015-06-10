# LOOP and WHILE Interpreter

LOOP and WHILE are pedagogical programming language designed by Uwe Schöning [see Wikipedia](https://en.wikipedia.org/wiki/LOOP_(programming_language\)).

This lets you run your LOOP and WHILE programs using a simple Haskell interpreter.

## Syntax

LOOP programs use the following syntax:
```
L ::= xn := xm + c
    | xn := xm - c
    | L; L
    | LOOP xn DO L END
```
where n, m, c are unsigend integer.
The conditional argument xn of a LOOP is evaluated only once before entering the loop, thus preventing infiinite loops.


WHILE programs use the following syntax:
```
W ::= xn := xm + c
    | xn := xm - c
    | W; W
    | WHILE xn DO W END
```
where n, m, c are unsigned integer.
The conditional argument xn of a WHILE is evaluated everytime the loop is run, infinite loops are possible.


### Examples
```
x0 + x1
```
can be expressed as
```
LOOP x1 DO
    x0 := x0 + 1
END
```
or
```
WHILE x1 DO
    x0 := x0 + 1
END
```
---
```
x0 * x1
```
as
```
x1 := x1 - 1;
LOOP x0 DO
    LOOP x1 DO
        x0 := x0 + 1
    END
END
```
or
```
x1 := x1 - 1;
WHILE x0 DO
    WHILE x1 DO
        x0 := x0 + 1
    END
END
```
---
However
```
f(x) = 0            if x = 0
       undefined    else
```
can not be expressed as a LOOP program. As a WHILE program it would be
```
WHILE x0 DO
    x0 := x0 + 0
END
```

## Usage

The parser uses [Happy](https://www.haskell.org/happy/) to generate the loop.hs file.

The Haskell file can either be used from GHCi offering:
```haskell
evalFile :: FilePath -> Map Int Int -> IO ()
-- Opens a given file and interprets it in the context of preset variables.
-- Variables are given as a Map of Int -> Int (xn -> value)
-- Prints result

evalFileEmpty :: FilePath -> IO ()
-- Like evalFile only with all variables initialized at 0

evalString :: String -> Map Int Int -> Int
-- Like evalFile only using a manually entered program rather than file input.

evalStringEmpty :: String -> Int
-- Like evalString only again with all variables initalized at 0
```


It can also be compiled and used as a command line tool using
```
./loop filename [x0 x1 x2 ...]
```
