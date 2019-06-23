-- In your package.yaml, add the following:
--      dependencies:
--      OTHER DEPENDENCIES YOU HAVE
--      - tasty
--      - tasty-hunit
import           Test.Tasty
import           Test.Tasty.HUnit

-- import your AST
import           AST                            ( Program
                                                , Type
                                                , Result
                                                )
-- import your EVALUATE TYPE function
import           EvalType                       ( evalType )
-- import your EVALUATE VALUE function
import           EvalValue                      ( evalValue )

import           Util
import           ExamplesCore
import           ExamplesADT

-- makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
-- makeFun (fn, rt) ((p, t):pts) body =
--     let helper [] = body
--         helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
--         ts = map snd pts ++ [rt]
--     in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

-- callFun :: Expr -> [Expr] -> Expr
-- callFun f [e] = EApply f e
-- callFun f (e:es) = callFun (EApply f e) es

mkTestCase :: Example -> [TestTree]
mkTestCase (Example n p Nothing r) =
    [testCase (n ++ " [ill-typed]") $ evalType p @?= Nothing]
mkTestCase (Example n p (Just t) r) =
    [ testCase (n ++ " [well-typed]") $ evalType p @?= Just t
    , testCase (n ++ " [value]") $ evalValue p @?= r
    ]

mkTestSuite :: String -> [Example] -> TestTree
mkTestSuite n es = testGroup n $ es >>= mkTestCase

testTasks :: [TestTree]
testTasks =
    [ mkTestSuite "simple"  simple
    , mkTestSuite "complex" complex
    , mkTestSuite "adt"     adt
    ]

main :: IO ()
main = defaultMain $ testGroup "" testTasks
