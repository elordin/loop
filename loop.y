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
    'DO'    { TokenDo }
    'END'   { TokenEnd }

%%
Exp : var ':=' var '+' int      { AssP $1 $3 $5 }
    | var ':=' var '-' int      { AssM $1 $3 $5 }
    | Exp ';' Exp               { Seq  $1 $3 }
    | 'LOOP' var 'DO' Exp 'END' { Loop $2 $4}

{
type Var = Int
type Const = Int

data Exp
    = AssP Var Var Const
    | AssM Var Var Const
    | Seq Exp Exp
    | Loop Var Exp
    deriving Show

data Token
    = TokenVar Var
    | TokenConst Const
    | TokenAss
    | TokenPlus
    | TokenMinus
    | TokenSeq
    | TokenLoop
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
lexer ('D':'O':cs) = TokenDo : lexer cs
lexer ('E':'N':'D':cs) = TokenEnd : lexer cs
lexer ('x':cs) = lexVar cs
lexer (c:cs)
    | isDigit c = lexNum (c:cs)
lexer _ = error "Invalid syntax"

skipLine ('\n':cs) = lexer cs
skipLine (_:cs) = skipLine cs

lexVar cs = TokenVar (read num) : lexer rest
    where (num, rest) = span isDigit cs

lexNum cs = TokenConst (read num) : lexer rest
      where (num,rest) = span isDigit cs


parseError :: [Token] -> a
parseError tokens = error $ "Error parsing" ++ (show tokens)

--main = getContents >>= print . parse . lexer

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
        [] -> return ()
        (f:[]) -> evalFileEmpty f
        (f:t) -> do
            let (vals, _) = span isInteger t
                varVals = zip [0..] vals
                dict = foldr (\(k, v) m -> Map.insert k (read v) m) Map.empty varVals
            evalFile f dict

}
