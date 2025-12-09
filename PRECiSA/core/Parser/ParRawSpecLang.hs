{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Parser.ParRawSpecLang
  ( happyError
  , myLexer
  , pSpec
  ) where

import Prelude

import qualified AbsRawSpecLang
import Parser.LexRawSpecLang
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Double)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (AbsRawSpecLang.Id)
	| HappyAbsSyn7 (AbsRawSpecLang.LBound)
	| HappyAbsSyn8 (AbsRawSpecLang.UBound)
	| HappyAbsSyn9 ([AbsRawSpecLang.VarBind])
	| HappyAbsSyn10 (AbsRawSpecLang.VarBind)
	| HappyAbsSyn11 ([AbsRawSpecLang.Id])
	| HappyAbsSyn12 (AbsRawSpecLang.SpecBind)
	| HappyAbsSyn13 ([AbsRawSpecLang.SpecBind])
	| HappyAbsSyn14 (AbsRawSpecLang.Spec)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m =
	   Prelude.Int
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
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int
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
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)]
	-> HappyStk HappyAbsSyn
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,92) ([0,2048,0,64,0,0,2080,0,32768,0,0,0,0,0,0,0,0,0,1,8192,0,192,0,0,1024,0,128,0,4,0,16,0,256,0,32,0,6,512,0,3096,0,64,0,8,0,0,0,0,0,0,1024,0,0,16384,0,2048,0,0,0,0,16384,0,0,24,0,0,0,0,0,16384,385,24576,48,3072,6,64,0,8,0,0,0,0,32768,0,0,0,12288,0,0,0,0,0,0,640,3,24656,0,128,0,16,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pSpec","Double","Integer","Id","LBound","UBound","ListVarBind","VarBind","ListId","SpecBind","ListSpecBind","Spec","'('","')'","'+inf'","','","'-'","'-inf'","':'","'['","']'","'`'","'in'","L_doubl","L_integ","L_Id","%eof"]
        bit_start = st Prelude.* 29
        bit_end = (st Prelude.+ 1) Prelude.* 29
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..28]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (28) = happyShift action_7
action_0 (6) = happyGoto action_3
action_0 (12) = happyGoto action_4
action_0 (13) = happyGoto action_5
action_0 (14) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (26) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (15) = happyShift action_9
action_3 (21) = happyShift action_10
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (28) = happyShift action_7
action_4 (6) = happyGoto action_3
action_4 (12) = happyGoto action_4
action_4 (13) = happyGoto action_8
action_4 _ = happyReduce_23

action_5 _ = happyReduce_25

action_6 (29) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_3

action_8 _ = happyReduce_24

action_9 (28) = happyShift action_7
action_9 (6) = happyGoto action_14
action_9 (11) = happyGoto action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (28) = happyShift action_7
action_10 (6) = happyGoto action_11
action_10 (9) = happyGoto action_12
action_10 (10) = happyGoto action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (24) = happyShift action_19
action_11 (25) = happyShift action_20
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_22

action_13 (18) = happyShift action_18
action_13 _ = happyReduce_14

action_14 (18) = happyShift action_17
action_14 _ = happyReduce_19

action_15 (16) = happyShift action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (21) = happyShift action_27
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (28) = happyShift action_7
action_17 (6) = happyGoto action_14
action_17 (11) = happyGoto action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (28) = happyShift action_7
action_18 (6) = happyGoto action_11
action_18 (9) = happyGoto action_25
action_18 (10) = happyGoto action_13
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (27) = happyShift action_24
action_19 (28) = happyShift action_7
action_19 (5) = happyGoto action_22
action_19 (6) = happyGoto action_23
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (22) = happyShift action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (19) = happyShift action_34
action_21 (20) = happyShift action_35
action_21 (26) = happyShift action_2
action_21 (27) = happyShift action_24
action_21 (4) = happyGoto action_31
action_21 (5) = happyGoto action_32
action_21 (7) = happyGoto action_33
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (25) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (25) = happyShift action_29
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_2

action_25 _ = happyReduce_15

action_26 _ = happyReduce_20

action_27 (28) = happyShift action_7
action_27 (6) = happyGoto action_11
action_27 (9) = happyGoto action_28
action_27 (10) = happyGoto action_13
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_21

action_29 (22) = happyShift action_40
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (22) = happyShift action_39
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_5

action_32 _ = happyReduce_4

action_33 (18) = happyShift action_38
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (26) = happyShift action_2
action_34 (27) = happyShift action_24
action_34 (4) = happyGoto action_36
action_34 (5) = happyGoto action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_8

action_36 _ = happyReduce_7

action_37 _ = happyReduce_6

action_38 (17) = happyShift action_46
action_38 (19) = happyShift action_47
action_38 (26) = happyShift action_2
action_38 (27) = happyShift action_24
action_38 (4) = happyGoto action_43
action_38 (5) = happyGoto action_44
action_38 (8) = happyGoto action_45
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (19) = happyShift action_34
action_39 (20) = happyShift action_35
action_39 (26) = happyShift action_2
action_39 (27) = happyShift action_24
action_39 (4) = happyGoto action_31
action_39 (5) = happyGoto action_32
action_39 (7) = happyGoto action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (19) = happyShift action_34
action_40 (20) = happyShift action_35
action_40 (26) = happyShift action_2
action_40 (27) = happyShift action_24
action_40 (4) = happyGoto action_31
action_40 (5) = happyGoto action_32
action_40 (7) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (18) = happyShift action_52
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (18) = happyShift action_51
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_10

action_44 _ = happyReduce_9

action_45 (23) = happyShift action_50
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_13

action_47 (26) = happyShift action_2
action_47 (27) = happyShift action_24
action_47 (4) = happyGoto action_48
action_47 (5) = happyGoto action_49
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_12

action_49 _ = happyReduce_11

action_50 _ = happyReduce_16

action_51 (17) = happyShift action_46
action_51 (19) = happyShift action_47
action_51 (26) = happyShift action_2
action_51 (27) = happyShift action_24
action_51 (4) = happyGoto action_43
action_51 (5) = happyGoto action_44
action_51 (8) = happyGoto action_54
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (17) = happyShift action_46
action_52 (19) = happyShift action_47
action_52 (26) = happyShift action_2
action_52 (27) = happyShift action_24
action_52 (4) = happyGoto action_43
action_52 (5) = happyGoto action_44
action_52 (8) = happyGoto action_53
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (23) = happyShift action_56
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (23) = happyShift action_55
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_17

action_56 _ = happyReduce_18

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn4
		 ((read happy_var_1) :: Double
	)
happyReduction_1 _  = notHappyAtAll

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read happy_var_1) :: Integer
	)
happyReduction_2 _  = notHappyAtAll

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_Id happy_var_1)))
	 =  HappyAbsSyn6
		 (AbsRawSpecLang.Id happy_var_1
	)
happyReduction_3 _  = notHappyAtAll

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (AbsRawSpecLang.LBInt happy_var_1
	)
happyReduction_4 _  = notHappyAtAll

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (AbsRawSpecLang.LBDouble happy_var_1
	)
happyReduction_5 _  = notHappyAtAll

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (AbsRawSpecLang.LBNegInt happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (AbsRawSpecLang.LBNegDouble happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (AbsRawSpecLang.LInf
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 (AbsRawSpecLang.UBInt happy_var_1
	)
happyReduction_9 _  = notHappyAtAll

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (AbsRawSpecLang.UBDouble happy_var_1
	)
happyReduction_10 _  = notHappyAtAll

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (AbsRawSpecLang.UBNegInt happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (AbsRawSpecLang.UBNegDouble happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn8
		 (AbsRawSpecLang.UInf
	)

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ((:[]) happy_var_1
	)
happyReduction_14 _  = notHappyAtAll

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll

happyReduce_16 = happyReduce 7 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsRawSpecLang.VarSpec happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 9 10 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsRawSpecLang.VarSpecIdx happy_var_1 happy_var_3 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 9 10 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsRawSpecLang.VarSpecField happy_var_1 happy_var_3 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn11
		 ((:[]) happy_var_1
	)
happyReduction_19 _  = notHappyAtAll

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn11
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll

happyReduce_21 = happyReduce 6 12 happyReduction_21
happyReduction_21 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsRawSpecLang.SpecBindN happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 (AbsRawSpecLang.SpecBind0 happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ((:[]) happy_var_1
	)
happyReduction_23 _  = notHappyAtAll

happyReduce_24 = happySpecReduce_2  13 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsRawSpecLang.Specification happy_var_1
	)
happyReduction_25 _  = notHappyAtAll

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 15;
	PT _ (TS _ 2) -> cont 16;
	PT _ (TS _ 3) -> cont 17;
	PT _ (TS _ 4) -> cont 18;
	PT _ (TS _ 5) -> cont 19;
	PT _ (TS _ 6) -> cont 20;
	PT _ (TS _ 7) -> cont 21;
	PT _ (TS _ 8) -> cont 22;
	PT _ (TS _ 9) -> cont 23;
	PT _ (TS _ 10) -> cont 24;
	PT _ (TS _ 11) -> cont 25;
	PT _ (TD happy_dollar_dollar) -> cont 26;
	PT _ (TI happy_dollar_dollar) -> cont 27;
	PT _ (T_Id happy_dollar_dollar) -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pSpec tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts)
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
