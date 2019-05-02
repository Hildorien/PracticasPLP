module ParserLC where
import Data.Char
import Exp
import Control.Monad (liftM, ap)
import Control.Applicative

-- parser produced by Happy Version 1.17

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 (Exp ())

type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33 :: () => Int -> HappyReduction (HappyIdentity)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13 :: () => HappyReduction (HappyIdentity)

action_0 (6) = happyShift action_2
action_0 (7) = happyShift action_5
action_0 (8) = happyShift action_6
action_0 (9) = happyShift action_7
action_0 (10) = happyShift action_8
action_0 (11) = happyShift action_9
action_0 (12) = happyShift action_10
action_0 (13) = happyShift action_11
action_0 (16) = happyShift action_12
action_0 (18) = happyShift action_13
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 _ = happyFail

action_1 (6) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (20) = happyAccept
action_3 _ = happyReduce_12

action_4 (6) = happyShift action_2
action_4 (7) = happyShift action_5
action_4 (8) = happyShift action_6
action_4 (9) = happyShift action_7
action_4 (10) = happyShift action_8
action_4 (11) = happyShift action_9
action_4 (12) = happyShift action_10
action_4 (13) = happyShift action_11
action_4 (16) = happyShift action_12
action_4 (18) = happyShift action_13
action_4 (4) = happyGoto action_20
action_4 (5) = happyGoto action_4
action_4 _ = happyFail

action_5 _ = happyReduce_2

action_6 (18) = happyShift action_19
action_6 _ = happyFail

action_7 (18) = happyShift action_18
action_7 _ = happyFail

action_8 (18) = happyShift action_17
action_8 _ = happyFail

action_9 _ = happyReduce_6

action_10 _ = happyReduce_7

action_11 (6) = happyShift action_2
action_11 (7) = happyShift action_5
action_11 (8) = happyShift action_6
action_11 (9) = happyShift action_7
action_11 (10) = happyShift action_8
action_11 (11) = happyShift action_9
action_11 (12) = happyShift action_10
action_11 (13) = happyShift action_11
action_11 (16) = happyShift action_12
action_11 (18) = happyShift action_13
action_11 (4) = happyGoto action_16
action_11 (5) = happyGoto action_4
action_11 _ = happyFail

action_12 (6) = happyShift action_15
action_12 _ = happyFail

action_13 (6) = happyShift action_2
action_13 (7) = happyShift action_5
action_13 (8) = happyShift action_6
action_13 (9) = happyShift action_7
action_13 (10) = happyShift action_8
action_13 (11) = happyShift action_9
action_13 (12) = happyShift action_10
action_13 (13) = happyShift action_11
action_13 (16) = happyShift action_12
action_13 (18) = happyShift action_13
action_13 (4) = happyGoto action_14
action_13 (5) = happyGoto action_4
action_13 _ = happyFail

action_14 (19) = happyShift action_26
action_14 _ = happyReduce_12

action_15 (17) = happyShift action_25
action_15 _ = happyFail

action_16 (14) = happyShift action_24
action_16 _ = happyReduce_12

action_17 (6) = happyShift action_2
action_17 (7) = happyShift action_5
action_17 (8) = happyShift action_6
action_17 (9) = happyShift action_7
action_17 (10) = happyShift action_8
action_17 (11) = happyShift action_9
action_17 (12) = happyShift action_10
action_17 (13) = happyShift action_11
action_17 (16) = happyShift action_12
action_17 (18) = happyShift action_13
action_17 (4) = happyGoto action_23
action_17 (5) = happyGoto action_4
action_17 _ = happyFail

action_18 (6) = happyShift action_2
action_18 (7) = happyShift action_5
action_18 (8) = happyShift action_6
action_18 (9) = happyShift action_7
action_18 (10) = happyShift action_8
action_18 (11) = happyShift action_9
action_18 (12) = happyShift action_10
action_18 (13) = happyShift action_11
action_18 (16) = happyShift action_12
action_18 (18) = happyShift action_13
action_18 (4) = happyGoto action_22
action_18 (5) = happyGoto action_4
action_18 _ = happyFail

action_19 (6) = happyShift action_2
action_19 (7) = happyShift action_5
action_19 (8) = happyShift action_6
action_19 (9) = happyShift action_7
action_19 (10) = happyShift action_8
action_19 (11) = happyShift action_9
action_19 (12) = happyShift action_10
action_19 (13) = happyShift action_11
action_19 (16) = happyShift action_12
action_19 (18) = happyShift action_13
action_19 (4) = happyGoto action_21
action_19 (5) = happyGoto action_4
action_19 _ = happyFail

action_20 (6) = happyReduce_13
action_20 (7) = happyReduce_13
action_20 (8) = happyReduce_13
action_20 (9) = happyReduce_13
action_20 (10) = happyReduce_13
action_20 (11) = happyReduce_13
action_20 (12) = happyReduce_13
action_20 (13) = happyReduce_13
action_20 (16) = happyReduce_13
action_20 (18) = happyReduce_13
action_20 _ = happyReduce_10

action_21 (19) = happyShift action_31
action_21 _ = happyReduce_12

action_22 (19) = happyShift action_30
action_22 _ = happyReduce_12

action_23 (19) = happyShift action_29
action_23 _ = happyReduce_12

action_24 (6) = happyShift action_2
action_24 (7) = happyShift action_5
action_24 (8) = happyShift action_6
action_24 (9) = happyShift action_7
action_24 (10) = happyShift action_8
action_24 (11) = happyShift action_9
action_24 (12) = happyShift action_10
action_24 (13) = happyShift action_11
action_24 (16) = happyShift action_12
action_24 (18) = happyShift action_13
action_24 (4) = happyGoto action_28
action_24 (5) = happyGoto action_4
action_24 _ = happyFail

action_25 (6) = happyShift action_2
action_25 (7) = happyShift action_5
action_25 (8) = happyShift action_6
action_25 (9) = happyShift action_7
action_25 (10) = happyShift action_8
action_25 (11) = happyShift action_9
action_25 (12) = happyShift action_10
action_25 (13) = happyShift action_11
action_25 (16) = happyShift action_12
action_25 (18) = happyShift action_13
action_25 (4) = happyGoto action_27
action_25 (5) = happyGoto action_4
action_25 _ = happyFail

action_26 _ = happyReduce_11

action_27 (6) = happyReduce_12
action_27 (7) = happyReduce_12
action_27 (8) = happyReduce_12
action_27 (9) = happyReduce_12
action_27 (10) = happyReduce_12
action_27 (11) = happyReduce_12
action_27 (12) = happyReduce_12
action_27 (13) = happyReduce_12
action_27 (16) = happyReduce_12
action_27 (18) = happyReduce_12
action_27 _ = happyReduce_9

action_28 (15) = happyShift action_32
action_28 _ = happyReduce_12

action_29 _ = happyReduce_5

action_30 _ = happyReduce_4

action_31 _ = happyReduce_3

action_32 (6) = happyShift action_2
action_32 (7) = happyShift action_5
action_32 (8) = happyShift action_6
action_32 (9) = happyShift action_7
action_32 (10) = happyShift action_8
action_32 (11) = happyShift action_9
action_32 (12) = happyShift action_10
action_32 (13) = happyShift action_11
action_32 (16) = happyShift action_12
action_32 (18) = happyShift action_13
action_32 (4) = happyGoto action_33
action_32 (5) = happyGoto action_4
action_32 _ = happyFail

action_33 (6) = happyReduce_12
action_33 (7) = happyReduce_12
action_33 (8) = happyReduce_12
action_33 (9) = happyReduce_12
action_33 (10) = happyReduce_12
action_33 (11) = happyReduce_12
action_33 (12) = happyReduce_12
action_33 (13) = happyReduce_12
action_33 (16) = happyReduce_12
action_33 (18) = happyReduce_12
action_33 _ = happyReduce_8

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (VarExp happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (ZeroExp
	)

happyReduce_3 = happyReduce 4 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SuccExp happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PredExp happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IsZeroExp happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn4
		 (TrueExp
	)

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn4
		 (FalseExp
	)

happyReduce_8 = happyReduce 6 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfExp happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 4 happyReduction_9
happyReduction_9 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LamExp happy_var_2 () happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (AppExp happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (AppExp happy_var_1 happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 20 20 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenVar happy_dollar_dollar -> cont 6;
	TokenZero -> cont 7;
	TokenSucc -> cont 8;
	TokenPred -> cont 9;
	TokenIsZero -> cont 10;
	TokenTrue -> cont 11;
	TokenFalse -> cont 12;
	TokenIf -> cont 13;
	TokenThen -> cont 14;
	TokenElse -> cont 15;
	TokenLam -> cont 16;
	TokenArrow -> cont 17;
	TokenOB -> cont 18;
	TokenCB -> cont 19;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

instance Functor HappyIdentity where
    fmap = liftM

instance Applicative HappyIdentity where
    pure = return
    (<*>) = ap

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [Token] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parseLCTokens tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError _ = error ("Parse error\n")






data Token
  = TokenVar String
  | TokenZero
  | TokenSucc
  | TokenPred
  | TokenIsZero
  | TokenTrue
  | TokenFalse
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenLam
  | TokenArrow
  | TokenOB
  | TokenCB
      deriving Show



lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
lexer ('0':cs) = TokenZero : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('.':cs) = TokenArrow : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexVar cs =
   case fstToLower $ span (\x->isAlpha x || isDigit x) cs of
     ("suc",rest) -> TokenSucc : lexer rest
     ("succ",rest) -> TokenSucc : lexer rest
     ("pred",rest) -> TokenPred : lexer rest
     ("iszero",rest)  -> TokenIsZero : lexer rest
     ("true",rest)  -> TokenTrue : lexer rest
     ("false",rest)  -> TokenFalse : lexer rest
     ("if",rest)  -> TokenIf : lexer rest
     ("then",rest)  -> TokenThen : lexer rest
     ("else",rest)  -> TokenElse : lexer rest
     ("lambda",rest)  -> TokenLam : lexer rest
     (var,rest)   -> TokenVar var : lexer rest
   where fstToLower (x, y) = (map toLower x, y)




parseLC :: String -> Exp ()
parseLC = parseLCTokens . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
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
