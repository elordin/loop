{
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar $$ }
    int     { TokenConst $$ }
    ':='    { TokenAss }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    ';'     { TokenSeq }
    'LOOP'  { TokenLoop }
    'WHILE' { TokenWhile }
    'DO'    { TokenDo }
    'END'   { TokenEnd }

%%
Exp : var ':=' var '+' int      { AssP $1 $3 $5 }
    | var ':=' var '-' int      { AssM $1 $3 $5 }
    | Exp ';' Exp               { Seq  $1 $3 }
    | 'LOOP' var 'DO' Exp 'END' { Loop $2 $4}
    | 'WHILE' var 'DO' Exp 'END' { While $2 $4}

{
type Var = Int
type Const = Int

data Exp
    = AssP Var Var Const
    | AssM Var Var Const
    | Seq Exp Exp
    | Loop Var Exp
    | While Var Exp
    deriving Show

data Token
    = TokenVar Var
    | TokenConst Const
    | TokenAss
    | TokenPlus
    | TokenMinus
    | TokenSeq
    | TokenLoop
    | TokenWhile
    | TokenDo
    | TokenEnd
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('#':cs) = skipLine cs
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('\r':cs) = lexer cs
lexer (':':'=':cs) = TokenAss : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer (';':cs) = TokenSeq : lexer cs
lexer ('L':'O':'O':'P':cs) = TokenLoop : lexer cs
lexer ('W':'H':'I':'L':'E':cs) = TokenWhile : lexer cs
lexer ('D':'O':cs) = TokenDo : lexer cs
lexer ('E':'N':'D':cs) = TokenEnd : lexer cs
lexer ('x':cs) = lexVar cs
    where lexVar cs = let (num, rest) = span isDigit cs
                      in TokenVar (read num) : lexer rest
lexer (c:cs)
    | isDigit c = lexNum (c:cs)
    where lexNum cs = let (num,rest) = span isDigit cs
                      in TokenConst (read num) : lexer rest
lexer cs = error $ "Invalid syntax near " ++ take 5 cs ++ "..."
skipLine ('\n':cs) = lexer cs
skipLine (_:cs) = skipLine cs

parseError :: [Token] -> a
parseError tokens = error $ "Error parsing" ++ (show tokens)

eval :: Exp -> Map.Map Var Const -> Const
eval exp initial = fromMaybe 0 $ Map.lookup 0 $ evalIntoDict exp initial
    where
        loop :: Int -> Exp -> Map.Map Var Const -> Map.Map Var Const
        loop 0 exp dict = dict
        loop n exp dict = loop (n - 1) exp $ evalIntoDict exp dict
        evalIntoDict :: Exp -> Map.Map Var Const -> Map.Map Var Const
        evalIntoDict (AssP x y c) dict = Map.insert x (max 0 $ (fromMaybe 0 $ Map.lookup y dict) + c) dict
        evalIntoDict (AssM x y c) dict = evalIntoDict (AssP x y (-1*c)) dict
        evalIntoDict (Seq e1 e2) dict = evalIntoDict e2 $ evalIntoDict e1 dict
        evalIntoDict l@(Loop cond exp) dict = loop (fromMaybe 0 $ Map.lookup cond dict) exp dict
        evalIntoDict l@(While cond exp) dict
            | (fromMaybe 0 $ Map.lookup cond dict) == 0 = dict
            | otherwise = evalIntoDict l $ evalIntoDict exp dict

evalString :: String -> Map.Map Var Const -> Const
evalString s dict = eval (parse $ lexer $ s) dict

evalStringEmpty :: String -> Const
evalStringEmpty s = evalString s Map.empty

evalFile :: FilePath -> Map.Map Var Const -> IO ()
evalFile file dict = do
    s <- readFile file
    print $ evalString s dict

evalFileEmpty :: FilePath -> IO ()
evalFileEmpty file = evalFile file Map.empty

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "LOOP / WHILE interpreter"
            putStrLn "    Usage: "
            putStrLn "        loop filename [x0 x1 x2 ...]\n"
            putStrLn "    LOOP Syntax:"
            putStrLn "        L ::= xn := xm + c"
            putStrLn "            | xn := xm - c"
            putStrLn "            | L; L"
            putStrLn "            | LOOP xn DO L END\n"
            putStrLn "    WHILE Syntax:"
            putStrLn "        W ::= xn := xm + c"
            putStrLn "            | xn := xm - c"
            putStrLn "            | W; W"
            putStrLn "            | WHILE xn DO W END\n"
        (f:[]) -> evalFileEmpty f
        (f:t) -> do
            let (vals, _) = span isInteger t
                varVals = zip [0..] vals
                dict = foldr (\(k, v) m -> Map.insert k (read v) m) Map.empty varVals
            evalFile f dict

}
