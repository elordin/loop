{-# OPTIONS_GHC -w #-}
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

action_0 (5) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (5) = happyShift action_2
action_1 _ = happyFail

action_2 (7) = happyShift action_9
action_2 _ = happyFail

action_3 (10) = happyShift action_8
action_3 (14) = happyAccept
action_3 _ = happyFail

action_4 (7) = happyShift action_7
action_4 _ = happyFail

action_5 (5) = happyShift action_6
action_5 _ = happyFail

action_6 (12) = happyShift action_13
action_6 _ = happyFail

action_7 (5) = happyShift action_12
action_7 _ = happyFail

action_8 (5) = happyShift action_4
action_8 (11) = happyShift action_5
action_8 (4) = happyGoto action_11
action_8 _ = happyFail

action_9 (5) = happyShift action_10
action_9 _ = happyFail

action_10 (8) = happyShift action_15
action_10 _ = happyFail

action_11 (10) = happyShift action_8
action_11 _ = happyReduce_3

action_12 (8) = happyShift action_15
action_12 (9) = happyShift action_16
action_12 _ = happyFail

action_13 (5) = happyShift action_4
action_13 (11) = happyShift action_5
action_13 (4) = happyGoto action_14
action_13 _ = happyFail

action_14 (10) = happyShift action_8
action_14 (13) = happyShift action_19
action_14 _ = happyFail

action_15 (6) = happyShift action_18
action_15 _ = happyFail

action_16 (6) = happyShift action_17
action_16 _ = happyFail

action_17 _ = happyReduce_2

action_18 _ = happyReduce_1

action_19 _ = happyReduce_4

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyTerminal (TokenConst happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (AssP happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 5 4 happyReduction_2
happyReduction_2 ((HappyTerminal (TokenConst happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (AssM happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq  happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Loop happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 14 14 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenVar happy_dollar_dollar -> cont 5;
	TokenConst happy_dollar_dollar -> cont 6;
	TokenAss -> cont 7;
	TokenPlus -> cont 8;
	TokenMinus -> cont 9;
	TokenSeq -> cont 10;
	TokenLoop -> cont 11;
	TokenDo -> cont 12;
	TokenEnd -> cont 13;
	_ -> happyError' (tk:tks)
	}

happyError_ 14 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure    = return
    a <*> b = (fmap id a) <*> b
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
