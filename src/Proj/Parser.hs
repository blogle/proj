{-# OPTIONS_GHC -w #-}
module Proj.Parser (
  parseExpr,
) where

import Control.Monad.Except
import Proj.Lexer
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

action_0 (9) = happyShift action_4
action_0 (13) = happyShift action_5
action_0 (15) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_3
action_0 _ = happyFail

action_1 (9) = happyShift action_4
action_1 (13) = happyShift action_5
action_1 (15) = happyShift action_6
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 _ = happyFail

action_2 (9) = happyShift action_4
action_2 (13) = happyShift action_5
action_2 (15) = happyShift action_6
action_2 (4) = happyGoto action_2
action_2 (5) = happyGoto action_8
action_2 _ = happyReduce_3

action_3 (10) = happyShift action_11
action_3 _ = happyReduce_1

action_4 _ = happyReduce_7

action_5 (9) = happyShift action_4
action_5 (13) = happyShift action_5
action_5 (15) = happyShift action_6
action_5 (4) = happyGoto action_2
action_5 (5) = happyGoto action_10
action_5 _ = happyFail

action_6 (17) = happyShift action_9
action_6 _ = happyFail

action_7 (9) = happyShift action_4
action_7 (13) = happyShift action_5
action_7 (15) = happyShift action_6
action_7 (19) = happyAccept
action_7 (4) = happyGoto action_2
action_7 (5) = happyGoto action_8
action_7 _ = happyReduce_3

action_8 (9) = happyReduce_2
action_8 (10) = happyShift action_11
action_8 (13) = happyReduce_2
action_8 (14) = happyReduce_2
action_8 (15) = happyReduce_2
action_8 (16) = happyReduce_2
action_8 (18) = happyReduce_2
action_8 (19) = happyReduce_2
action_8 _ = happyReduce_2

action_9 (9) = happyShift action_4
action_9 (13) = happyShift action_5
action_9 (15) = happyShift action_6
action_9 (4) = happyGoto action_2
action_9 (5) = happyGoto action_14
action_9 _ = happyFail

action_10 (10) = happyShift action_11
action_10 (14) = happyShift action_13
action_10 _ = happyReduce_1

action_11 (9) = happyShift action_4
action_11 (13) = happyShift action_5
action_11 (15) = happyShift action_6
action_11 (4) = happyGoto action_2
action_11 (5) = happyGoto action_12
action_11 _ = happyFail

action_12 (9) = happyReduce_5
action_12 (10) = happyShift action_11
action_12 (13) = happyReduce_5
action_12 (14) = happyReduce_5
action_12 (15) = happyReduce_5
action_12 (16) = happyReduce_5
action_12 (18) = happyReduce_5
action_12 (19) = happyReduce_5
action_12 _ = happyReduce_5

action_13 _ = happyReduce_6

action_14 (10) = happyShift action_11
action_14 (18) = happyShift action_15
action_14 _ = happyReduce_1

action_15 (9) = happyShift action_4
action_15 (13) = happyShift action_5
action_15 (15) = happyShift action_6
action_15 (4) = happyGoto action_2
action_15 (5) = happyGoto action_16
action_15 _ = happyFail

action_16 (10) = happyShift action_11
action_16 (16) = happyShift action_17
action_16 _ = happyReduce_1

action_17 _ = happyReduce_4

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Fun happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal (TokVerb happy_var_2))
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Op (opCode happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyTerminal (TokNum  happy_var_1))
	 =  HappyAbsSyn5
		 (Lit (LInteger happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 19 19 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokName happy_dollar_dollar -> cont 6;
	TokSym  happy_dollar_dollar -> cont 7;
	TokStr  happy_dollar_dollar -> cont 8;
	TokNum  happy_dollar_dollar -> cont 9;
	TokVerb happy_dollar_dollar -> cont 10;
	TokAdverb happy_dollar_dollar -> cont 11;
	TokAssign -> cont 12;
	TokLParen -> cont 13;
	TokRParen -> cont 14;
	TokLBrace -> cont 15;
	TokRBrace -> cont 16;
	TokLBrack -> cont 17;
	TokRBrack -> cont 18;
	_ -> happyError' (tk:tks)
	}

happyError_ 19 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
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

expr tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Literal
	= LBool String
	| LInteger String
	| LFloat String
	| LString String
    | LSymbol String

data Verb
  = Add
  | Sub
  | Mul
  | Div

data Expr 
	= Var String
    | Op Verb Expr Expr
    | App Expr Expr
	| List [Expr]
    | Lit Literal
    | Fun Expr Expr
    -- | Lambda IFunc EnvContext

-- data IFunc = IFunc [Expr] [Expr]-- IFunc { fn :: [Expr] -> Eval Expr}

opCode :: String -> Verb
opCode "+" = Add
opCode "-" = Sub
opCode "%" = Div
opCode "*" = Mul

parseExpr :: String -> Expr
parseExpr = expr . scanTokens

parseError :: [Token] -> a
parseError _ = error "ParseError!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/home/ogleb/.stack/programs/x86_64-linux/ghc-ncurses6-8.0.2/lib/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc1830_0/ghc_2.h" #-}






















































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 256 "templates/GenericTemplate.hs" #-}
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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
