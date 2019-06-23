module ExamplesCore where

import           Util
import           AST

-- simple cases

makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
makeFun (fn, rt) ((p, t):pts) body =
    let helper [] = body
        helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
        ts = map snd pts ++ [rt]
    in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

callFun :: Expr -> [Expr] -> Expr
callFun f [e] = EApply f e
callFun f (e:es) = callFun (EApply f e) es

expr_Int_succ = ELambda ("x", TInt) (EAdd (EVar "x") (EIntLit 1))
expr_Int_succ_bad = ELambda ("x", TBool) (EAdd (EVar "x") (EIntLit 1))
expr_even = ELambda ("x", TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))
expr_fact = EIf
    (EEq (EVar "x") (EIntLit 0))
    (EIntLit 1)
    (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1))))

simple =
    [ Example "l00_EBoolLit"
              (Program [] $ EBoolLit True)
              (Just TBool)
              (RBool True)
    , Example "l01_EIntLit"
              (Program [] $ EIntLit (-42))
              (Just TInt)
              (RInt (-42))
    , Example "l02_ECharLit"
              (Program [] $ ECharLit '@')
              (Just TChar)
              (RChar '@')
    , Example "l03_ENot"
              (Program [] $ ENot (EBoolLit False))
              (Just TBool)
              (RBool True)
    , Example "l03_ENot_bad"
              (Program [] $ ENot (EIntLit (-42)))
              (Nothing)
              (RInvalid)
    , Example "l04_EAnd"
              (Program [] $ EAnd (EBoolLit True) (EBoolLit False))
              (Just TBool)
              (RBool False)
    , Example "l04_EAnd_bad"
              (Program [] $ EAnd (EBoolLit True) (EIntLit (-42)))
              (Nothing)
              (RInvalid)
    , Example "l05_EOr"
              (Program [] $ EOr (EBoolLit False) (EBoolLit True))
              (Just TBool)
              (RBool True)
    , Example "l05_EOr_bad"
              (Program [] $ EOr (EBoolLit False) (EIntLit (-42)))
              (Nothing)
              (RInvalid)
    , Example "l06_EAdd"
              (Program [] $ EAdd (EIntLit 40) (EIntLit 2))
              (Just TInt)
              (RInt 42)
    , Example "l06_EAdd_bad"
              (Program [] $ EAdd (EIntLit 40) (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example "l07_ESub"
              (Program [] $ ESub (EIntLit 40) (EIntLit 2))
              (Just TInt)
              (RInt 38)
    , Example "l07_ESub_bad"
              (Program [] $ ESub (EIntLit 40) (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example "l08_EMul"
              (Program [] $ EMul (EIntLit 6) (EIntLit 7))
              (Just TInt)
              (RInt 42)
    , Example "l08_EMul_bad"
              (Program [] $ EMul (EIntLit 40) (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example "l09_EDiv"
              (Program [] $ EDiv (EIntLit 85) (EIntLit 2))
              (Just TInt)
              (RInt 42)
    , Example "l09_EDiv_bad"
              (Program [] $ EDiv (EIntLit 40) (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example "l10_EMod"
              (Program [] $ EMod (EIntLit 19) (EIntLit 5))
              (Just TInt)
              (RInt 4)
    , Example "l10_EMod_bad"
              (Program [] $ EMod (EIntLit 40) (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example "l11_EEq"
              (Program [] $ EEq (EIntLit 42) (EIntLit 42))
              (Just TBool)
              (RBool True)
    , Example "l11_EEq_bad"
              (Program [] $ EEq (EIntLit 0) (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example "l12_ENeq"
              (Program [] $ ENeq (ECharLit '@') (ECharLit '@'))
              (Just TBool)
              (RBool False)
    , Example "l12_ENeq_bad"
              (Program [] $ ENeq (ECharLit '0') (EIntLit 48))
              (Nothing)
              (RInvalid)
    , Example "l13_ELt"
              (Program [] $ ELt (ECharLit 'A') (ECharLit 'a'))
              (Just TBool)
              (RBool True)
    , Example "l13_ELt_bad"
              (Program [] $ ELt (EBoolLit False) (EBoolLit True))
              (Nothing)
              (RInvalid)
    , Example "l14_EGt"
              (Program [] $ EGt (ECharLit 'a') (ECharLit 'A'))
              (Just TBool)
              (RBool True)
    , Example "l14_EGt_bad"
              (Program [] $ EGt (ECharLit '0') (EIntLit 48))
              (Nothing)
              (RInvalid)
    , Example "l15_ELe"
              (Program [] $ ELe (ECharLit '@') (ECharLit '@'))
              (Just TBool)
              (RBool True)
    , Example "l15_ELe_bad"
              (Program [] $ ELe (ECharLit '0') (EIntLit 48))
              (Nothing)
              (RInvalid)
    , Example "l16_EGe"
              (Program [] $ EGe (EIntLit 4) (EIntLit 2))
              (Just TBool)
              (RBool True)
    , Example "l16_EGe_bad"
              (Program [] $ EGe (ECharLit '0') (EIntLit 48))
              (Nothing)
              (RInvalid)
    , Example "l17_EIf"
              (Program [] $ EIf (EBoolLit False) (EIntLit 42) (EIntLit 233))
              (Just TInt)
              (RInt 233)
    , Example "l17_EIf_0_bad"
              (Program [] $ EIf (ECharLit 'T') (EIntLit 42) (EIntLit 233))
              (Nothing)
              (RInvalid)
    , Example "l17_EIf_1_bad"
              (Program [] $ EIf (EBoolLit True) (ECharLit '0') (EIntLit 48))
              (Nothing)
              (RInvalid)
    , Example "l18_ELambda_only_typecheck"
              (Program [] expr_Int_succ)
              (Just (TArrow TInt TInt))
              (RInvalid)
    , Example "l18_ELambda_bad"
              (Program [] expr_Int_succ_bad)
              (Nothing)
              (RInvalid)
    , Example "l19_EApply"
              (Program [] $ EApply expr_Int_succ (EIntLit 41))
              (Just TInt)
              (RInt 42)
    , Example "l19_EApply_bad"
              (Program [] $ EApply expr_Int_succ (EBoolLit False))
              (Nothing)
              (RInvalid)
    , Example
        "l20_ELet_0"
        (Program [] $ ELet ("x", EIntLit 41) (EAdd (EVar "x") (EIntLit 1)))
        (Just TInt)
        (RInt 42)
    , Example
        "l20_ELet_0_bad"
        (Program [] $ ELet ("x", EBoolLit False) (EAdd (EVar "x") (EIntLit 1)))
        (Nothing)
        (RInvalid)
    , Example
        "l20_ELet_1"
        ( Program []
        $ ELet ("even", expr_even) (EApply (EVar "even") (EIntLit 42))
        )
        (Just TBool)
        (RBool True)
    , Example
        "l20_ELet_1_bad"
        ( Program []
        $ ELet ("even", expr_even) (EApply (EVar "even") (ECharLit '9'))
        )
        (Nothing)
        (RInvalid)
    , Example
        "l21_ELetRec_0"
        (Program [] $ ELetRec "fact"
                              ("x"      , TInt)
                              (expr_fact, TInt)
                              (EApply (EVar "fact") (EIntLit 5))
        )
        (Just TInt)
        (RInt 120)
    , Example
        "l21_ELetRec_0_0_bad"
        (Program [] $ ELetRec "fact"
                              ("x"      , TInt)
                              (expr_fact, TBool)
                              (EApply (EVar "fact") (EIntLit 5))
        )
        (Nothing)
        (RInvalid)
    , Example
        "l21_ELetRec_0_1_bad"
        (Program [] $ ELetRec "fact"
                              ("x"      , TBool)
                              (expr_fact, TInt)
                              (EApply (EVar "fact") (EIntLit 5))
        )
        (Nothing)
        (RInvalid)
    , Example
        "l21_ELetRec_0_2_bad"
        (Program [] $ ELetRec "fact"
                              ("x"      , TInt)
                              (expr_fact, TInt)
                              (EApply (EVar "fact") (EBoolLit True))
        )
        (Nothing)
        (RInvalid)
    ]

-- complex cases

complex =
    [ Example
        "00_aplusb"
        ( Program []
        $ makeFun ("solution", TInt)
                  [("a", TInt), ("b", TInt)]
                  (EAdd (EVar "a") (EVar "b"))
        $ ELet ("a", EIntLit 1)
        $ ELet ("b", EIntLit 2)
        $ ELet ("c", EIntLit 3)
        $ ELet ("d", callFun (EVar "solution") [EVar "b", EVar "c"])
        $ callFun (EVar "solution") [EVar "a", EVar "d"]
        )
        (Just TInt)
        (RInt 6)
    , Example
        "01_lcm"
        ( Program []
        $ makeFun
              ("gcd", TInt)
              [("a", TInt), ("b", TInt)]
              (EIf
                  (EEq (EVar "a") (EVar "b"))
                  (EVar "a")
                  (EIf
                      (EGt (EVar "a") (EVar "b"))
                      (callFun (EVar "gcd")
                               [ESub (EVar "a") (EVar "b"), EVar "b"]
                      )
                      (callFun (EVar "gcd")
                               [ESub (EVar "b") (EVar "a"), EVar "a"]
                      )
                  )
              )
        $ makeFun
              ("solution", TInt)
              [("a", TInt), ("b", TInt)]
              ( ELet ("c", callFun (EVar "gcd") [EVar "a", EVar "b"])
              $ EDiv (EMul (EVar "a") (EVar "b")) (EVar "c")
              )
        $ callFun (EVar "solution") [EIntLit 27, EIntLit 36]
        )
        (Just TInt)
        (RInt 108)
    , Example
        "02_church0"
        ( Program []
        $ ELet
              ( "zero"
              , ELambda ("f", TArrow TInt TInt)
                        (ELambda ("x", TInt) (EVar "x"))
              )
        $ ELet
              ( "succ"
              , ELambda
                  ("n", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda
                      ("f", TArrow TInt TInt)
                      (ELambda
                          ("x", TInt)
                          (EApply
                              (EVar "f")
                              (callFun (EVar "n") [EVar "f", EVar "x"])
                          )
                      )
                  )
              )
        $ ELet
              ( "plus"
              , ELambda
                  ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda
                      ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                      (ELambda
                          ("f", TArrow TInt TInt)
                          (ELambda
                              ("x", TInt)
                              (ELet
                                  ("af", EApply (EVar "a") (EVar "f"))
                                  (ELet
                                      ("bf", EApply (EVar "b") (EVar "f"))
                                      (EApply
                                          (EVar "af")
                                          (EApply (EVar "bf") (EVar "x"))
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
        $ ELet
              ( "mult"
              , ELambda
                  ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda
                      ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                      (ELambda
                          ("f", TArrow TInt TInt)
                          (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))
                      )
                  )
              )
        $ ELet ("f", ELambda ("x", TInt) (EAdd (EVar "x") (EIntLit 1)))
        $ ELet ("one", EApply (EVar "succ") (EVar "zero"))
        $ ELet ("two", EApply (EVar "succ") (EVar "one"))
        $ ELet ("three", EApply (EVar "succ") (EVar "two"))
        $ ELet ("five", callFun (EVar "plus") [EVar "two", EVar "three"])
        $ ELet ("six", callFun (EVar "mult") [EVar "two", EVar "three"])
        $ EAdd (callFun (EVar "six") [EVar "f", EIntLit 0])
               (callFun (EVar "five") [EVar "f", EIntLit 0])
        )
        (Just TInt)
        (RInt 11)
    , Example
        "04_church2"
        ( Program []
        $ ELet
              ( "zero"
              , ELambda ("f", TArrow TInt TInt)
                        (ELambda ("x", TInt) (EVar "x"))
              )
        $ ELet
              ( "succ"
              , ELambda
                  ("n", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda
                      ("f", TArrow TInt TInt)
                      (ELambda
                          ("x", TInt)
                          (EApply
                              (EVar "f")
                              (callFun (EVar "n") [EVar "f", EVar "x"])
                          )
                      )
                  )
              )
        $ ELet
              ( "mult"
              , ELambda
                  ("a", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                  (ELambda
                      ("b", TArrow (TArrow TInt TInt) (TArrow TInt TInt))
                      (ELambda
                          ("f", TArrow TInt TBool)
                          (EApply (EVar "b") (EApply (EVar "a") (EVar "f")))
                      )
                  )
              )
        $ (EVar "mult")
        )
        (Nothing)
        (RInvalid)
    , Example
        "06_and"
        ( Program []
        $ makeFun
              ("test", TBool)
              [("x", TBool)]
              (EOr (EVar "x") (callFun (EVar "test") [EBoolLit False]))
        $ callFun (EVar "test") [EBoolLit True]
        )
        (Just TBool)
        (RBool True)
    , Example
        "08_apply"
        ( Program []
        $ makeFun ("sum3", TInt)
                  [("x1", TInt), ("x2", TInt), ("x3", TInt)]
                  (EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3"))
        $ callFun (EVar "sum3") [EIntLit 1, EIntLit 1, EIntLit 1, EIntLit 1]
        )
        (Nothing)
        (RInvalid)
    , Example
        "09_apply"
        ( Program []
        $ makeFun ("sum3", TInt)
                  [("x1", TInt), ("x2", TInt), ("x3", TInt)]
                  (EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3"))
        $ ELet ("f", callFun (EVar "sum3") [EIntLit 1, EIntLit 1])
        $ EApply (EVar "f") (EIntLit 1)
        )
        (Just TInt)
        (RInt 3)
    ]
