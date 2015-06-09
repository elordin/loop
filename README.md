# LOOP Interpreter

LOOP is a pedagogical programming language designed by Uwe Schöning [see Wikipedia](https://en.wikipedia.org/wiki/LOOP_(programming_language\)).

This lets you run your LOOP programs using a simple Haskell interpreter.

## Syntax

LOOP programs use the following syntax:
```
P ::= xn := xm + c
    | xn := xm - c
    | P; P
    | LOOP xn DO P END
```
where n, m, c are unsigend integer

For example
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
