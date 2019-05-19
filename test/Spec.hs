{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import System.Timeout(timeout)

import AST
import EvalValue
import EvalType


-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06
main = htfMain testsWithTimeouts

-- HTF default convention: testcases start with `test_`.
-- If `stack test` fails on Windows, try removing all unicode characters in `Spec.hs`.

------------------------------------------------------------------

------------- Simple Cases Start -------------

-- The program
tRaw_l00_EBoolLit = 
  Program [] $ EBoolLit True

-- The expected value
tRaw_l00_EBoolLit_value = RBool True
-- The expected type
tRaw_l00_EBoolLit_type = Just TBool


tRaw_l01_EIntLit = 
  Program [] $ EIntLit (-42)

tRaw_l01_EIntLit_value = RInt (-42)
tRaw_l01_EIntLit_type = Just TInt


tRaw_l02_ECharLit = 
  Program [] $ ECharLit '@'

tRaw_l02_ECharLit_value = RChar '@'
tRaw_l02_ECharLit_type = Just TChar


tRaw_l03_ENot = 
  Program [] $ ENot (EBoolLit False)

tRaw_l03_ENot_value = RBool True
tRaw_l03_ENot_type = Just TBool

-- The program, not well-typed
tRaw_l03_ENot_bad = 
  Program [] $ ENot (EIntLit (-42))

-- The expected type (Nothing, because the program is not well-typed)
tRaw_l03_ENot_bad_type = Nothing


tRaw_l04_EAnd = 
  Program [] $ EAnd (EBoolLit True) (EBoolLit False)

tRaw_l04_EAnd_value = RBool False
tRaw_l04_EAnd_type = Just TBool


tRaw_l04_EAnd_bad = 
  Program [] $ EAnd (EBoolLit True) (EIntLit (-42))

tRaw_l04_EAnd_bad_type = Nothing


tRaw_l05_EOr = 
  Program [] $ EOr (EBoolLit False) (EBoolLit True)

tRaw_l05_EOr_value = RBool True
tRaw_l05_EOr_type = Just TBool


tRaw_l05_EOr_bad = 
  Program [] $ EOr (EBoolLit False) (EIntLit (-42))

tRaw_l05_EOr_bad_type = Nothing


tRaw_l06_EAdd = 
  Program [] $ EAdd (EIntLit 40) (EIntLit 2)

tRaw_l06_EAdd_value = RInt 42
tRaw_l06_EAdd_type = Just TInt


tRaw_l06_EAdd_bad = 
  Program [] $ EAdd (EIntLit 40) (EBoolLit False)

tRaw_l06_EAdd_bad_type = Nothing


tRaw_l07_ESub = 
  Program [] $ ESub (EIntLit 40) (EIntLit 2)

tRaw_l07_ESub_value = RInt 38
tRaw_l07_ESub_type = Just TInt

tRaw_l07_ESub_bad = 
  Program [] $ ESub (EIntLit 40) (EBoolLit False)

tRaw_l07_ESub_bad_type = Nothing


tRaw_l08_EMul = 
  Program [] $ EMul (EIntLit 6) (EIntLit 7)

tRaw_l08_EMul_value = RInt 42
tRaw_l08_EMul_type = Just TInt


tRaw_l08_EMul_bad = 
  Program [] $ EMul (EIntLit 40) (EBoolLit False)

tRaw_l08_EMul_bad_type = Nothing


tRaw_l09_EDiv = 
  Program [] $ EDiv (EIntLit 85) (EIntLit 2)

tRaw_l09_EDiv_value = RInt 42
tRaw_l09_EDiv_type = Just TInt

tRaw_l09_EDiv_bad = 
  Program [] $ EDiv (EIntLit 40) (EBoolLit False)

tRaw_l09_EDiv_bad_type = Nothing


tRaw_l10_EMod = 
  Program [] $ EMod (EIntLit 19) (EIntLit 5)

tRaw_l10_EMod_value = RInt 4
tRaw_l10_EMod_type = Just TInt


tRaw_l10_EMod_bad = 
  Program [] $ EMod (EIntLit 40) (EBoolLit False)

tRaw_l10_EMod_bad_type = Nothing


tRaw_l11_EEq = 
  Program [] $ EEq (EIntLit 42) (EIntLit 42)

tRaw_l11_EEq_value = RBool True
tRaw_l11_EEq_type = Just TBool


tRaw_l11_EEq_bad = 
  Program [] $ EEq (EIntLit 0) (EBoolLit False)

tRaw_l11_EEq_bad_type = Nothing


tRaw_l12_ENeq = 
  Program [] $ ENeq (ECharLit '@') (ECharLit '@')

tRaw_l12_ENeq_value = RBool False
tRaw_l12_ENeq_type = Just TBool


tRaw_l12_ENeq_bad = 
  Program [] $ ENeq (ECharLit '0') (EIntLit 48)

tRaw_l12_ENeq_bad_type = Nothing


-- | Unicode
tRaw_l13_ELt = 
  Program [] $ ELt (ECharLit 'A') (ECharLit 'a')

tRaw_l13_ELt_value = RBool True
tRaw_l13_ELt_type = Just TBool

-- Boolean type in this language is "Eq", but not "Ord".
-- TODO or, leave it as UB?
tRaw_l13_ELt_bad = 
  Program [] $ ELt (EBoolLit False) (EBoolLit True)

tRaw_l13_ELt_bad_type = Nothing


tRaw_l14_EGt = 
  Program [] $ EGt (ECharLit 'a') (ECharLit 'A')

tRaw_l14_EGt_value = RBool True
tRaw_l14_EGt_type = Just TBool


tRaw_l14_EGt_bad = 
  Program [] $ EGt (ECharLit '0') (EIntLit 48)

tRaw_l14_EGt_bad_type = Nothing


tRaw_l15_ELe = 
  Program [] $ ELe (ECharLit '@') (ECharLit '@')

tRaw_l15_ELe_value = RBool True
tRaw_l15_ELe_type = Just TBool


tRaw_l15_ELe_bad = 
  Program [] $ ELe (ECharLit '0') (EIntLit 48)

tRaw_l15_ELe_bad_type = Nothing


tRaw_l16_EGe = 
  Program [] $ EGe (EIntLit 4) (EIntLit 2)

tRaw_l16_EGe_value = RBool True
tRaw_l16_EGe_type = Just TBool


tRaw_l16_EGe_bad = 
  Program [] $ EGe (ECharLit '0') (EIntLit 48)

tRaw_l16_EGe_bad_type = Nothing


tRaw_l17_EIf = 
  Program [] $ EIf (EBoolLit False) (EIntLit 42) (EIntLit 233)

tRaw_l17_EIf_value = RInt 233
tRaw_l17_EIf_type = Just TInt


-- the condition is not of boolean type
tRaw_l17_EIf_bad_0 = 
  Program [] $ EIf (ECharLit 'T') (EIntLit 42) (EIntLit 233)

tRaw_l17_EIf_bad_0_type = Nothing


-- `then` branch and `else` branch are of different types
tRaw_l17_EIf_bad_1 = 
  Program [] $ EIf (EBoolLit True) (ECharLit '0') (EIntLit 48)

tRaw_l17_EIf_bad_1_type = Nothing


-- An expression, which is an anonymous function, like \x -> x+1 in Haskell (but the parameter type is specified to TInt)
expr_Int_succ = ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))
-- You may want to write a `evalTypeInternal :: Expr -> Maybe Type` 
-- to evaluate that `evalTypeInternal expr_Int_succ == Just (TArrow TInt TInt)`

tRaw_l18_ELambda_only_typecheck = 
  Program [] expr_Int_succ

tRaw_l17_EIf_ELambda_only_typecheck_type = Just (TArrow TInt TInt)


expr_Int_succ_bad = ELambda ("x",TBool) (EAdd (EVar "x") (EIntLit 1))

tRaw_l18_ELambda_bad = 
  Program [] expr_Int_succ_bad

tRaw_l18_ELambda_bad_type = Nothing


tRaw_l19_EApply = 
  Program [] $ EApply expr_Int_succ (EIntLit 41)

tRaw_l19_EApply_value = RInt 42
tRaw_l19_EApply_type = Just TInt


tRaw_l19_EApply_bad =
  Program [] $ EApply expr_Int_succ (EBoolLit False)

tRaw_l19_EApply_bad_type = Nothing


-- let x = 41 in x + 1
tRaw_l20_ELet_0 = 
  Program [] $ ELet ("x", EIntLit 41) (EAdd (EVar "x") (EIntLit 1))

tRaw_l20_ELet_0_value = RInt 42
tRaw_l20_ELet_0_type = Just TInt

-- let x = False in x + 1
tRaw_l20_ELet_0_bad = 
  Program [] $ ELet ("x", EBoolLit False) (EAdd (EVar "x") (EIntLit 1))

tRaw_l20_ELet_0_bad_type = Nothing


-- let even = (\x -> x `mod` 2 == 0) in even 42
expr_even = ELambda ("x",TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))
tRaw_l20_ELet_1 = 
  Program [] $ ELet ("even", expr_even) (EApply (EVar "even") (EIntLit 42))

tRaw_l20_ELet_1_value = RBool True
tRaw_l20_ELet_1_type = Just TBool

-- let even = (\x -> x `mod` 2 == 0) in even '9'
tRaw_l20_ELet_1_bad =
  Program [] $ ELet ("even", expr_even) (EApply (EVar "even") (ECharLit '9'))

tRaw_l20_ELet_1_bad_type = Nothing


-- let fact = (\x -> if x == 0 then 1 else x * fact (x-1)) :: Int -> Int in fact 5
expr_fact = EIf (EEq (EVar "x") (EIntLit 0))
  (EIntLit 1)
  (EMul 
    (EVar "x") 
    (EApply 
      (EVar "fact") 
      (ESub (EVar "x") (EIntLit 1))
      )
    )
  
tRaw_l21_ELetRec_0 = 
  Program [] $ ELetRec "fact" ("x", TInt) (expr_fact, TInt) (EApply (EVar "fact") (EIntLit 5))

tRaw_l21_ELetRec_0_value = RInt 120
tRaw_l21_ELetRec_0_type = Just TInt

-- let fact = (\x -> if x == 0 then 1 else x * fact (x-1)) :: Int -> Bool in fact 5
tRaw_l21_ELetRec_0_bad_0 = 
  Program [] $ ELetRec "fact" ("x", TInt) (expr_fact, TBool) (EApply (EVar "fact") (EIntLit 5))

tRaw_l21_ELetRec_0_bad_0_type = Nothing

-- let fact = (\x -> if x == 0 then 1 else x * fact (x-1)) :: Bool -> Int in fact 5
tRaw_l21_ELetRec_0_bad_1 = 
  Program [] $ ELetRec "fact" ("x", TBool) (expr_fact, TInt) (EApply (EVar "fact") (EIntLit 5))

tRaw_l21_ELetRec_0_bad_1_type = Nothing

-- let fact = (\x -> if x == 0 then 1 else x * fact (x-1)) :: Int -> Int in fact True
tRaw_l21_ELetRec_0_bad_2 = 
  Program [] $ ELetRec "fact" ("x", TInt) (expr_fact, TInt) (EApply (EVar "fact") (EBoolLit True))

tRaw_l21_ELetRec_0_bad_2_type = Nothing

-------------- Simple Cases End --------------

-------------- Complex Cases Start --------------

-- | Shortcut to define function with @ELetRec@
makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
makeFun (fn, rt) ((p, t):pts) body =
  let helper [] = body
      helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
      ts = map snd pts ++ [rt]
  in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

callFun :: Expr -> [Expr] -> Expr
callFun f [e] = EApply f e
callFun f (e:es) = callFun (EApply f e) es


-- homework 1.1
tRaw_00_aplusb =
  Program [] $

  makeFun ("solution", TInt) [("a", TInt), ("b", TInt)]
  (
    EAdd (EVar "a") (EVar "b")
  ) $

  ELet ("a", EIntLit 1) $
  ELet ("b", EIntLit 2) $
  ELet ("c", EIntLit 3) $
  ELet ("d", callFun (EVar "solution") [EVar "b", EVar "c"]) $
  callFun (EVar "solution") [EVar "a", EVar "d"]

tRaw_00_aplusb_value = RInt 6
tRaw_00_aplusb_type = Just TInt


-- homework 1.2
tRaw_01_lcm =
  Program [] $

  makeFun ("gcd", TInt) [("a", TInt), ("b", TInt)]
  (
    EIf (EEq (EVar "a") (EVar "b"))
    (EVar "a")
    (EIf (EGt (EVar "a") (EVar "b"))
      (callFun (EVar "gcd") [ESub (EVar "a") (EVar "b"), EVar "b"])
      (callFun (EVar "gcd") [ESub (EVar "b") (EVar "a"), EVar "a"]))
  ) $

  makeFun ("solution", TInt) [("a", TInt), ("b", TInt)]
  (
    ELet ("c", callFun (EVar "gcd") [EVar "a", EVar "b"]) $
    EDiv (EMul (EVar "a") (EVar "b")) (EVar "c")
  ) $

  callFun (EVar "solution") [EIntLit 27, EIntLit 36]

tRaw_01_lcm_value = RInt 108
tRaw_01_lcm_type = Just TInt


tRaw_02_church0 =
  Program [] $

  ELet ("zero", ELambda ("f", TArrow TInt TInt)
                (ELambda ("x", TInt) (EVar "x"))) $
  ELet ("succ", ELambda ("n", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("f", TArrow TInt TInt)
                  (ELambda ("x", TInt)
                    (EApply (EVar "f")
                      (callFun (EVar "n") [EVar "f", EVar "x"]))))) $
  -- plus = \a b f x -> a f (b f x)
  ELet ("plus", ELambda ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda ("f", TArrow TInt TInt)
                    (ELambda ("x", TInt)
                      (ELet ("af", EApply (EVar "a") (EVar "f"))
                        (ELet ("bf", EApply (EVar "b") (EVar "f"))
                          (EApply (EVar "af") (EApply (EVar "bf") (EVar "x"))))))))) $
  -- mult = \a b f -> b (a f)
  ELet ("mult", ELambda ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda ("f", TArrow TInt TInt)
                    (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))))) $

  ELet ("f", ELambda ("x", TInt) (EAdd (EVar "x") (EIntLit 1))) $
  ELet ("one", EApply (EVar "succ") (EVar "zero")) $
  ELet ("two", EApply (EVar "succ") (EVar "one")) $
  ELet ("three", EApply (EVar "succ") (EVar "two")) $
  ELet ("five", callFun (EVar "plus") [EVar "two", EVar "three"]) $
  ELet ("six", callFun (EVar "mult") [EVar "two", EVar "three"]) $
  EAdd
  (callFun (EVar "six") [EVar "f", EIntLit 0])
  (callFun (EVar "five") [EVar "f", EIntLit 0])

tRaw_02_church0_value = RInt 11
tRaw_02_church0_type = Just TInt


tRaw_04_church2 =
  Program [] $

  ELet ("zero", ELambda ("f", TArrow TInt TInt)
                (ELambda ("x", TInt) (EVar "x"))) $
  ELet ("succ", ELambda ("n", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("f", TArrow TInt TInt)
                  (ELambda ("x", TInt)
                    (EApply (EVar "f")
                      (callFun (EVar "n") [EVar "f", EVar "x"]))))) $
  -- mult = \a b f -> b (a f)
  ELet ("mult", ELambda ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                (ELambda ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda ("f", TArrow TInt TBool)
                    (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))))) $

  (EVar "mult")

tRaw_04_church2_type = Nothing


-- | It can terminate under short-circuit evaluation.
-- It doesn't terminate under strict evaluation.
tRaw_06_and =
  Program [] $

  makeFun ("test", TBool) [("x", TBool)]
  (
    EOr (EVar "x") (callFun (EVar "test") [EBoolLit False])
  ) $

  callFun (EVar "test") [EBoolLit True]

tRaw_06_and_value = RBool True
tRaw_06_and_type = Just TBool


tRaw_08_apply =
  Program [] $

  makeFun ("sum3", TInt) [("x1", TInt), ("x2", TInt), ("x3", TInt)]
  (
    EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3")
  ) $

  callFun (EVar "sum3") [EIntLit 1, EIntLit 1, EIntLit 1, EIntLit 1]

tRaw_08_apply_type = Nothing


tRaw_09_apply =
  Program [] $

  makeFun ("sum3", TInt) [("x1", TInt), ("x2", TInt), ("x3", TInt)]
  (
    EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3")
  ) $

  ELet ("f", callFun (EVar "sum3") [EIntLit 1, EIntLit 1]) $
  EApply (EVar "f") (EIntLit 1)

tRaw_09_apply_value = RInt 3
tRaw_09_apply_type = Just TInt

-------------- Complex Cases End --------------

---------- Real HTF test cases start ----------

-- literals
test_simple_00_EBoolLit_value = assertEqual (tRaw_l00_EBoolLit_value) (EvalValue.evalValue tRaw_l00_EBoolLit)
test_simple_00_EBoolLit_type = assertEqual (tRaw_l00_EBoolLit_type) (EvalType.evalType tRaw_l00_EBoolLit)
test_simple_01_EIntLit_value = assertEqual (tRaw_l01_EIntLit_value) (EvalValue.evalValue tRaw_l01_EIntLit)
test_simple_01_EIntLit_type = assertEqual (tRaw_l01_EIntLit_type) (EvalType.evalType tRaw_l01_EIntLit)
test_simple_02_ECharLit_value = assertEqual (tRaw_l02_ECharLit_value) (EvalValue.evalValue tRaw_l02_ECharLit)
test_simple_02_ECharLit_type = assertEqual (tRaw_l02_ECharLit_type) (EvalType.evalType tRaw_l02_ECharLit)

-- boolean operators
test_simple_03_ENot_value = assertEqual (tRaw_l03_ENot_value) (EvalValue.evalValue tRaw_l03_ENot)
test_simple_03_ENot_type = assertEqual (tRaw_l03_ENot_type) (EvalType.evalType tRaw_l03_ENot)
test_simple_03_ENot_type_bad = assertEqual (tRaw_l03_ENot_bad_type) (EvalType.evalType tRaw_l03_ENot_bad)
test_simple_04_EAnd_value = assertEqual (tRaw_l04_EAnd_value) (EvalValue.evalValue tRaw_l04_EAnd)
test_simple_04_EAnd_type = assertEqual (tRaw_l04_EAnd_type) (EvalType.evalType tRaw_l04_EAnd)
test_simple_04_EAnd_type_bad = assertEqual (tRaw_l04_EAnd_bad_type) (EvalType.evalType tRaw_l04_EAnd_bad)
test_simple_05_EOr_value = assertEqual (tRaw_l05_EOr_value) (EvalValue.evalValue tRaw_l05_EOr)
test_simple_05_EOr_type = assertEqual (tRaw_l05_EOr_type) (EvalType.evalType tRaw_l05_EOr)
test_simple_05_EOr_type_bad = assertEqual (tRaw_l05_EOr_bad_type) (EvalType.evalType tRaw_l05_EOr_bad)

-- integer operators
test_simple_06_EAdd_value = assertEqual (tRaw_l06_EAdd_value) (EvalValue.evalValue tRaw_l06_EAdd)
test_simple_06_EAdd_type = assertEqual (tRaw_l06_EAdd_type) (EvalType.evalType tRaw_l06_EAdd)
test_simple_06_EAdd_type_bad = assertEqual (tRaw_l06_EAdd_bad_type) (EvalType.evalType tRaw_l06_EAdd_bad)
test_simple_07_ESub_value = assertEqual (tRaw_l07_ESub_value) (EvalValue.evalValue tRaw_l07_ESub)
test_simple_07_ESub_type = assertEqual (tRaw_l07_ESub_type) (EvalType.evalType tRaw_l07_ESub)
test_simple_07_ESub_type_bad = assertEqual (tRaw_l07_ESub_bad_type) (EvalType.evalType tRaw_l07_ESub_bad)
test_simple_08_EMul_value = assertEqual (tRaw_l08_EMul_value) (EvalValue.evalValue tRaw_l08_EMul)
test_simple_08_EMul_type = assertEqual (tRaw_l08_EMul_type) (EvalType.evalType tRaw_l08_EMul)
test_simple_08_EMul_type_bad = assertEqual (tRaw_l08_EMul_bad_type) (EvalType.evalType tRaw_l08_EMul_bad)
test_simple_09_EDiv_value = assertEqual (tRaw_l09_EDiv_value) (EvalValue.evalValue tRaw_l09_EDiv)
test_simple_09_EDiv_type = assertEqual (tRaw_l09_EDiv_type) (EvalType.evalType tRaw_l09_EDiv)
test_simple_09_EDiv_type_bad = assertEqual (tRaw_l09_EDiv_bad_type) (EvalType.evalType tRaw_l09_EDiv_bad)
test_simple_10_EMod_value = assertEqual (tRaw_l10_EMod_value) (EvalValue.evalValue tRaw_l10_EMod)
test_simple_10_EMod_type = assertEqual (tRaw_l10_EMod_type) (EvalType.evalType tRaw_l10_EMod)
test_simple_10_EMod_type_bad = assertEqual (tRaw_l10_EMod_bad_type) (EvalType.evalType tRaw_l10_EMod_bad)


-- "Eq" operators
test_simple_11_EEq_value = assertEqual (tRaw_l11_EEq_value) (EvalValue.evalValue tRaw_l11_EEq)
test_simple_11_EEq_type = assertEqual (tRaw_l11_EEq_type) (EvalType.evalType tRaw_l11_EEq)
test_simple_11_EEq_type_bad = assertEqual (tRaw_l11_EEq_bad_type) (EvalType.evalType tRaw_l11_EEq_bad)
test_simple_12_ENeq_value = assertEqual (tRaw_l12_ENeq_value) (EvalValue.evalValue tRaw_l12_ENeq)
test_simple_12_ENeq_type = assertEqual (tRaw_l12_ENeq_type) (EvalType.evalType tRaw_l12_ENeq)
test_simple_12_ENeq_type_bad = assertEqual (tRaw_l12_ENeq_bad_type) (EvalType.evalType tRaw_l12_ENeq_bad)

-- "Ord" operators
test_simple_13_ELt_value = assertEqual (tRaw_l13_ELt_value) (EvalValue.evalValue tRaw_l13_ELt)
test_simple_13_ELt_type = assertEqual (tRaw_l13_ELt_type) (EvalType.evalType tRaw_l13_ELt)
test_simple_13_ELt_type_bad = assertEqual (tRaw_l13_ELt_bad_type) (EvalType.evalType tRaw_l13_ELt_bad)
test_simple_14_EGt_value = assertEqual (tRaw_l14_EGt_value) (EvalValue.evalValue tRaw_l14_EGt)
test_simple_14_EGt_type = assertEqual (tRaw_l14_EGt_type) (EvalType.evalType tRaw_l14_EGt)
test_simple_14_EGt_type_bad = assertEqual (tRaw_l14_EGt_bad_type) (EvalType.evalType tRaw_l14_EGt_bad)
test_simple_15_ELe_value = assertEqual (tRaw_l15_ELe_value) (EvalValue.evalValue tRaw_l15_ELe)
test_simple_15_ELe_type = assertEqual (tRaw_l15_ELe_type) (EvalType.evalType tRaw_l15_ELe)
test_simple_15_ELe_type_bad = assertEqual (tRaw_l15_ELe_bad_type) (EvalType.evalType tRaw_l15_ELe_bad)
test_simple_16_EGe_value = assertEqual (tRaw_l16_EGe_value) (EvalValue.evalValue tRaw_l16_EGe)
test_simple_16_EGe_type = assertEqual (tRaw_l16_EGe_type) (EvalType.evalType tRaw_l16_EGe)
test_simple_16_EGe_type_bad = assertEqual (tRaw_l16_EGe_bad_type) (EvalType.evalType tRaw_l16_EGe_bad)

-- if then else. Two kinds of typing error
test_simple_17_EIf_value = assertEqual (tRaw_l17_EIf_value) (EvalValue.evalValue tRaw_l17_EIf)
test_simple_17_EIf_type = assertEqual (tRaw_l17_EIf_type) (EvalType.evalType tRaw_l17_EIf)
test_simple_17_EIf_type_bad_0 = assertEqual (tRaw_l17_EIf_bad_0_type) (EvalType.evalType tRaw_l17_EIf_bad_0)
test_simple_17_EIf_type_bad_1 = assertEqual (tRaw_l17_EIf_bad_1_type) (EvalType.evalType tRaw_l17_EIf_bad_1)

-- lambda expression (non-recursive function) definition. No value result!
-- no test_simple_18_ELambda_value
test_simple_18_ELambda_only_typecheck = assertEqual (tRaw_l17_EIf_ELambda_only_typecheck_type) (EvalType.evalType tRaw_l18_ELambda_only_typecheck)
test_simple_18_ELambda_type_bad = assertEqual (tRaw_l18_ELambda_bad_type) (EvalType.evalType tRaw_l18_ELambda_bad)
-- function application
test_simple_19_EApply_value = assertEqual (tRaw_l19_EApply_value) (EvalValue.evalValue tRaw_l19_EApply)
test_simple_19_EApply_type = assertEqual (tRaw_l19_EApply_type) (EvalType.evalType tRaw_l19_EApply)
test_simple_19_EApply_type_bad = assertEqual (tRaw_l19_EApply_bad_type) (EvalType.evalType tRaw_l19_EApply_bad)

-- let-in expression
test_simple_20_ELet_0_value = assertEqual (tRaw_l20_ELet_0_value) (EvalValue.evalValue tRaw_l20_ELet_0)
test_simple_20_ELet_0_type = assertEqual (tRaw_l20_ELet_0_type) (EvalType.evalType tRaw_l20_ELet_0)
test_simple_20_ELet_0_type_bad = assertEqual (tRaw_l20_ELet_0_bad_type) (EvalType.evalType tRaw_l20_ELet_0_bad)
test_simple_20_ELet_1_value = assertEqual (tRaw_l20_ELet_1_value) (EvalValue.evalValue tRaw_l20_ELet_1)
test_simple_20_ELet_1_type = assertEqual (tRaw_l20_ELet_1_type) (EvalType.evalType tRaw_l20_ELet_1)
test_simple_20_ELet_1_type_bad = assertEqual (tRaw_l20_ELet_1_bad_type) (EvalType.evalType tRaw_l20_ELet_1_bad)


-- maybe-recursive function definition
test_simple_21_ELetRec_0_value = assertEqual (tRaw_l21_ELetRec_0_value) (EvalValue.evalValue tRaw_l21_ELetRec_0)
test_simple_21_ELetRec_0_type = assertEqual (tRaw_l21_ELetRec_0_type) (EvalType.evalType tRaw_l21_ELetRec_0)
test_simple_21_ELetRec_0_type_bad_0 = assertEqual (tRaw_l21_ELetRec_0_bad_0_type) (EvalType.evalType tRaw_l21_ELetRec_0_bad_0)
test_simple_21_ELetRec_0_type_bad_1 = assertEqual (tRaw_l21_ELetRec_0_bad_1_type) (EvalType.evalType tRaw_l21_ELetRec_0_bad_1)
test_simple_21_ELetRec_0_type_bad_2 = assertEqual (tRaw_l21_ELetRec_0_bad_2_type) (EvalType.evalType tRaw_l21_ELetRec_0_bad_2)


test_complex_00_aplusb_value = assertEqual (tRaw_00_aplusb_value) (EvalValue.evalValue tRaw_00_aplusb)
test_complex_00_aplusb_type = assertEqual (tRaw_00_aplusb_type) (EvalType.evalType tRaw_00_aplusb)
test_complex_01_lcm_value = assertEqual (tRaw_01_lcm_value) (EvalValue.evalValue tRaw_01_lcm)
test_complex_01_lcm_type = assertEqual (tRaw_01_lcm_type) (EvalType.evalType tRaw_01_lcm)
test_complex_02_church0_value = assertEqual (tRaw_02_church0_value) (EvalValue.evalValue tRaw_02_church0)
test_complex_02_church0_type = assertEqual (tRaw_02_church0_type) (EvalType.evalType tRaw_02_church0)

-- no test_complex_04_church2_value
test_complex_04_church2_type = assertEqual (tRaw_04_church2_type) (EvalType.evalType tRaw_04_church2)

test_complex_06_and_value = assertEqual (tRaw_06_and_value) (EvalValue.evalValue tRaw_06_and)
test_complex_06_and_type = assertEqual (tRaw_06_and_type) (EvalType.evalType tRaw_06_and)

-- no test_complex_08_apply_value
test_complex_08_apply_type = assertEqual (tRaw_08_apply_type) (EvalType.evalType tRaw_08_apply)
test_complex_09_apply_value = assertEqual (tRaw_09_apply_value) (EvalValue.evalValue tRaw_09_apply)
test_complex_09_apply_type = assertEqual (tRaw_09_apply_type) (EvalType.evalType tRaw_09_apply)

---------- Real HTF test cases end ----------

-- my own tests
tMy_01_EDiv_div0 =
  Program [] $ EDiv (EIntLit 1) (EIntLit 0)
tMy_01_EDiv_div0_value = RInvalid
test_my_simple_01_EDiv = assertEqual (tMy_01_EDiv_div0_value) (EvalValue.evalValue tMy_01_EDiv_div0)

tMy_02_EMod_mod0 =
  Program [] $ EMod (EIntLit 1) (EIntLit 0)
tMy_02_EMod_mod0_value = RInvalid
test_my_simple_02_EMod = assertEqual (tMy_02_EMod_mod0_value) (EvalValue.evalValue tMy_02_EMod_mod0)

tMy_03_EDiv_bad =
  Program [] $ EDiv (EBoolLit True) (EIntLit 0)
tMy_03_EDiv_bad_type = Nothing
test_my_simple_03_EDiv = assertEqual (tMy_03_EDiv_bad_type) (EvalType.evalType tMy_03_EDiv_bad)