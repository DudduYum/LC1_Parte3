{-# OPTIONS_GHC -w #-}
module Main where
import OberonLexer
import OberonTools
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

action_0 (29) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_6
action_0 (8) = happyGoto action_3
action_0 _ = happyFail

action_1 (29) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (8) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (69) = happyShift action_9
action_3 _ = happyFail

action_4 (74) = happyShift action_8
action_4 _ = happyFail

action_5 (79) = happyAccept
action_5 _ = happyFail

action_6 (69) = happyShift action_7
action_6 _ = happyReduce_1

action_7 (29) = happyShift action_4
action_7 (4) = happyGoto action_16
action_7 (5) = happyGoto action_6
action_7 (8) = happyGoto action_3
action_7 _ = happyFail

action_8 _ = happyReduce_7

action_9 (29) = happyShift action_4
action_9 (31) = happyShift action_14
action_9 (32) = happyShift action_15
action_9 (4) = happyGoto action_10
action_9 (5) = happyGoto action_6
action_9 (8) = happyGoto action_3
action_9 (9) = happyGoto action_11
action_9 (10) = happyGoto action_12
action_9 (11) = happyGoto action_13
action_9 _ = happyFail

action_10 _ = happyReduce_11

action_11 (74) = happyShift action_23
action_11 _ = happyFail

action_12 (29) = happyShift action_4
action_12 (32) = happyShift action_15
action_12 (4) = happyGoto action_10
action_12 (5) = happyGoto action_6
action_12 (8) = happyGoto action_3
action_12 (10) = happyGoto action_12
action_12 (11) = happyGoto action_22
action_12 _ = happyReduce_12

action_13 (31) = happyShift action_21
action_13 _ = happyFail

action_14 _ = happyReduce_8

action_15 (74) = happyShift action_20
action_15 (6) = happyGoto action_17
action_15 (7) = happyGoto action_18
action_15 (12) = happyGoto action_19
action_15 _ = happyFail

action_16 _ = happyReduce_2

action_17 (68) = happyShift action_26
action_17 _ = happyFail

action_18 (69) = happyShift action_25
action_18 _ = happyFail

action_19 _ = happyReduce_10

action_20 (67) = happyShift action_24
action_20 _ = happyReduce_4

action_21 _ = happyReduce_9

action_22 _ = happyReduce_13

action_23 _ = happyReduce_3

action_24 (74) = happyShift action_20
action_24 (6) = happyGoto action_33
action_24 _ = happyFail

action_25 (74) = happyShift action_20
action_25 (6) = happyGoto action_17
action_25 (7) = happyGoto action_18
action_25 (12) = happyGoto action_32
action_25 _ = happyReduce_14

action_26 (21) = happyShift action_28
action_26 (22) = happyShift action_29
action_26 (23) = happyShift action_30
action_26 (24) = happyShift action_31
action_26 (13) = happyGoto action_27
action_26 _ = happyFail

action_27 _ = happyReduce_6

action_28 _ = happyReduce_16

action_29 _ = happyReduce_17

action_30 _ = happyReduce_18

action_31 _ = happyReduce_19

action_32 _ = happyReduce_15

action_33 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1:happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 ((HappyTerminal (TokenVariableIdentifier happy_var_4)) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (do
                                                                          let newProc = happy_var_1
                                                                          if (procedureName newProc) == happy_var_4 then
                                                                            defaultDeclaration { declarationType = DT_Procedure, procedureDeclared = Just (addBodyToProcedure newProc happy_var_3) }
                                                                          else
                                                                            parseError [KW_TokenChar]
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal (TokenVariableIdentifier happy_var_1))
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenVariableIdentifier happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1:happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (createVariablesDefinitionsOfType happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyTerminal (TokenVariableIdentifier happy_var_2))
	_
	 =  HappyAbsSyn8
		 (defaultProcedure { procedureName = happy_var_2 }
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn9
		 ([]
	)

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  10 happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1++happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1++happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn13
		 (Simple Integer
	)

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn13
		 (Simple Float
	)

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn13
		 (Simple Boolean
	)

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn13
		 (Simple Char
	)

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1:[]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1:happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  20 happyReduction_27
happyReduction_27 (HappyTerminal (TokenIntegerNumber happy_var_1))
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 79 79 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	KW_TokenInteger -> cont 21;
	KW_TokenReal -> cont 22;
	KW_TokenBoolean -> cont 23;
	KW_TokenChar -> cont 24;
	KW_TokenSet -> cont 25;
	KW_TokenArray -> cont 26;
	KW_TokenOf -> cont 27;
	KW_TokenPointerTo -> cont 28;
	KW_TokenProcedure -> cont 29;
	KW_TokenBegin -> cont 30;
	KW_TokenEnd -> cont 31;
	KW_TokenVar -> cont 32;
	KW_TokenConst -> cont 33;
	KW_TokenTrue -> cont 34;
	KW_TokenFalse -> cont 35;
	KW_TokenIf -> cont 36;
	KW_TokenElsif -> cont 37;
	KW_TokenElse -> cont 38;
	KW_TokenThen -> cont 39;
	KW_TokenCase -> cont 40;
	KW_TokenWhile -> cont 41;
	KW_TokenDo -> cont 42;
	KW_TokenRepeat -> cont 43;
	KW_TokenUntil -> cont 44;
	KW_TokenLoop -> cont 45;
	KW_TokenExit -> cont 46;
	KW_TokenReturn -> cont 47;
	KW_TokenBreak -> cont 48;
	KW_TokenContinue -> cont 49;
	KW_TokenOr -> cont 50;
	KW_TokenCommercialE -> cont 51;
	KW_TokenTilde -> cont 52;
	KW_TokenPlus -> cont 53;
	KW_TokenMinus -> cont 54;
	KW_TokenStar -> cont 55;
	KW_TokenForwardSlash -> cont 56;
	KW_TokenDiv -> cont 57;
	KW_TokenMod -> cont 58;
	KW_TokenEquel -> cont 59;
	KW_TokenDiesis -> cont 60;
	KW_TokenMinor -> cont 61;
	KW_TokenMinorEqual -> cont 62;
	KW_TokenMajor -> cont 63;
	KW_TokenMajorEqual -> cont 64;
	KW_TokenAssignment -> cont 65;
	KW_TokenPoint -> cont 66;
	KW_TokenComa -> cont 67;
	KW_TokenColon -> cont 68;
	KW_TokenSemiColon -> cont 69;
	KW_TokenOpenBracket -> cont 70;
	KW_TokenClosedBracket -> cont 71;
	KW_TokenOpenSquareBracket -> cont 72;
	KW_TokenClosedSquareBracket -> cont 73;
	TokenVariableIdentifier happy_dollar_dollar -> cont 74;
	TokenIntegerNumber happy_dollar_dollar -> cont 75;
	TokenRealNumber happy_dollar_dollar -> cont 76;
	TokenValidChar happy_dollar_dollar -> cont 77;
	TokenValidString happy_dollar_dollar -> cont 78;
	_ -> happyError' (tk:tks)
	}

happyError_ 79 tk tks = happyError' tks
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

oLikeParse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
-- parseError st = error st

dStack = []

main = do
  inStr <- getContents
  --print (alexScanTokens inStr)
  let result = oLikeParse (alexScanTokens inStr)
  --oLikeParse (alexScanTokens inStr)
  putStrLn ("result: " ++ show(result))
  putStrLn("DONE")
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/usr/local/Cellar/ghc/7.10.3b/lib/ghc-7.10.3/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










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

