{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParDecisionPaths where
import AbsRawPVSLang (VarId(..), NonVarId(..))
import Common.DecisionPath
import Parser.LexDecisionPaths
import ErrM
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Integer)
	| HappyAbsSyn5 (VarId)
	| HappyAbsSyn6 (NonVarId)
	| HappyAbsSyn8 ([Integer])
	| HappyAbsSyn9 (LDecisionPath)
	| HappyAbsSyn10 ([LDecisionPath])
	| HappyAbsSyn12 ([VarId])
	| HappyAbsSyn13 ((String, [LDecisionPath]))
	| HappyAbsSyn14 ([(String, [LDecisionPath])])
	| HappyAbsSyn15 (TargetDPs)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

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
 action_32 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (26) = happyShift action_7
action_0 (6) = happyGoto action_3
action_0 (13) = happyGoto action_4
action_0 (14) = happyGoto action_5
action_0 (15) = happyGoto action_6
action_0 _ = happyFail

action_1 (24) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (16) = happyShift action_9
action_3 (19) = happyShift action_10
action_3 _ = happyFail

action_4 (26) = happyShift action_7
action_4 (6) = happyGoto action_3
action_4 (13) = happyGoto action_4
action_4 (14) = happyGoto action_8
action_4 _ = happyReduce_19

action_5 _ = happyReduce_21

action_6 (27) = happyAccept
action_6 _ = happyFail

action_7 _ = happyReduce_3

action_8 _ = happyReduce_20

action_9 (25) = happyShift action_19
action_9 (5) = happyGoto action_17
action_9 (12) = happyGoto action_18
action_9 _ = happyFail

action_10 (20) = happyShift action_14
action_10 (21) = happyShift action_15
action_10 (22) = happyShift action_16
action_10 (9) = happyGoto action_11
action_10 (10) = happyGoto action_12
action_10 (11) = happyGoto action_13
action_10 _ = happyReduce_9

action_11 (18) = happyShift action_25
action_11 _ = happyReduce_10

action_12 _ = happyReduce_12

action_13 _ = happyReduce_18

action_14 _ = happyReduce_13

action_15 _ = happyReduce_14

action_16 (24) = happyShift action_2
action_16 (4) = happyGoto action_22
action_16 (7) = happyGoto action_23
action_16 (8) = happyGoto action_24
action_16 _ = happyReduce_5

action_17 (18) = happyShift action_21
action_17 _ = happyReduce_15

action_18 (17) = happyShift action_20
action_18 _ = happyFail

action_19 _ = happyReduce_2

action_20 (19) = happyShift action_30
action_20 _ = happyFail

action_21 (25) = happyShift action_19
action_21 (5) = happyGoto action_17
action_21 (12) = happyGoto action_29
action_21 _ = happyFail

action_22 _ = happyReduce_4

action_23 (18) = happyShift action_28
action_23 _ = happyReduce_6

action_24 (23) = happyShift action_27
action_24 _ = happyFail

action_25 (22) = happyShift action_16
action_25 (9) = happyGoto action_11
action_25 (10) = happyGoto action_26
action_25 _ = happyReduce_9

action_26 _ = happyReduce_11

action_27 _ = happyReduce_8

action_28 (24) = happyShift action_2
action_28 (4) = happyGoto action_22
action_28 (7) = happyGoto action_23
action_28 (8) = happyGoto action_32
action_28 _ = happyReduce_5

action_29 _ = happyReduce_16

action_30 (20) = happyShift action_14
action_30 (21) = happyShift action_15
action_30 (22) = happyShift action_16
action_30 (9) = happyGoto action_11
action_30 (10) = happyGoto action_12
action_30 (11) = happyGoto action_31
action_30 _ = happyReduce_9

action_31 _ = happyReduce_17

action_32 _ = happyReduce_7

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn4
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (T_VarId happy_var_1)))
	 =  HappyAbsSyn5
		 (VarId (happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_NonVarId happy_var_1)))
	 =  HappyAbsSyn6
		 (NonVarId (happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  8 happyReduction_5
happyReduction_5  =  HappyAbsSyn8
		 ([]
	)

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ((:[]) happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 ++ (happy_var_1:[])
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (LDP happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  10 happyReduction_9
happyReduction_9  =  HappyAbsSyn10
		 ([]
	)

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:[]) happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn10
		 ([LDP []]
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn10
		 ([]
	)

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 ((:[]) happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn12
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 6 13 happyReduction_17
happyReduction_17 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (buildDecisionPaths happy_var_1 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn13
		 (buildDecisionPaths happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ((:[]) happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  14 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 16;
	PT _ (TS _ 2) -> cont 17;
	PT _ (TS _ 3) -> cont 18;
	PT _ (TS _ 4) -> cont 19;
	PT _ (TS _ 5) -> cont 20;
	PT _ (TS _ 6) -> cont 21;
	PT _ (TS _ 7) -> cont 22;
	PT _ (TS _ 8) -> cont 23;
	PT _ (TI happy_dollar_dollar) -> cont 24;
	PT _ (T_VarId happy_dollar_dollar) -> cont 25;
	PT _ (T_NonVarId happy_dollar_dollar) -> cont 26;
	_ -> happyError' (tk:tks)
	}

happyError_ 27 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pTargetDPs tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


buildDecisionPaths  :: NonVarId -> [LDecisionPath] -> (String, [LDecisionPath])
buildDecisionPaths (NonVarId fun) dps = (fun, dps)

rawparserTargetDPs :: String -> Err TargetDPs
rawparserTargetDPs = pTargetDPs . tokens

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}


















-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























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

