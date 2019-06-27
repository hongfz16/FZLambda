module Main where

import System.IO
import EvalType
import AST
import EvalValue
import LambdaParser

-- makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
-- makeFun (fn, rt) ((p, t):pts) body =
--   let helper [] = body
--       helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
--       ts = map snd pts ++ [rt]
--   in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

-- callFun :: Expr -> [Expr] -> Expr
-- callFun f [e] = EApply f e
-- callFun f (e:es) = callFun (EApply f e) es

-- prog =
--     Program [ADT "[int]" [("[]@int",[]),("::@int",[TInt,TData "[int]"])]] $
--     (ELetRec "length" ("xs",TData "[int]")
--       (ECase (EVar "xs") [
--           (PData "[]@int" [],EIntLit 0),
--           (PData "::@int" [PVar "_",PVar "xs'"],EAdd (EIntLit 1) (EApply (EVar "length") (EVar "xs'")))
--         ],TInt)
--       (ELetRec "filter" ("p",TArrow TInt TBool) 
--         (ELambda ("xs",TData "[int]")
--           (ECase (EVar "xs") [
--             (PData "[]@int" [],EVar "[]@int"),
--             (PData "::@int" [PVar "x",PVar "xs'"],EIf (EApply (EVar "p") (EVar "x")) (EApply (EApply (EVar "::@int") (EVar "x")) (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs'"))) (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs'")))
--           ]),
--           TArrow (TData "[int]") (TData "[int]"))
--         (ELet ("forall",ELambda ("p",TArrow TInt TBool) (ELambda ("xs",TData "[int]") (EEq (EApply (EVar "length") (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs"))) (EApply (EVar "length") (EVar "xs")))))
--           (EApply (EApply (EVar "forall") (ELambda ("x",TInt) (EGt (EVar "x") (EIntLit 0)))) (EApply (EApply (EVar "::@int") (EIntLit 1)) (EApply (EApply (EVar "::@int") (EIntLit 2)) (EApply (EApply (EVar "::@int") (EIntLit 3)) (EVar "[]@int"))))))
--       )
--     )

-- prog2 =
--     Program [ADT "[int]" [("[]@int",[]),("::@int",[TInt,TData "[int]"])]]
--     (ELetRec "filter" ("p",TArrow TInt TBool)
--         (ELambda ("xs",TData "[int]")
--             (ECase (EVar "xs")
--             [
--                 (PData "[]@int" [], EVar "[]@int"),
--                 (PData "::@int" [PVar "x", PVar "xs'"],
--                     EIf (EApply (EVar "p") (EVar "x"))
--                         (EApply (EApply (EVar "::@int") (EVar "x"))
--                             (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs'"))
--                         )
--                         (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs'"))
--                 )
--             ]
--             ),
--             TArrow (TData "[int]") (TData "[int]")
--         )
--         (ELetRec "sum" ("xs",TData "[int]")
--             (
--                 ECase (EVar "xs")
--                 [
--                     (PData "[]@int" [], EIntLit 0),
--                     (PData "::@int" [PVar "x",PVar "xs'"],
--                         EAdd (EVar "x") (EApply (EVar "sum") (EVar "xs'"))
--                     )
--                 ],
--                 TInt
--             )
--             (
--                 EApply (EVar "sum") 
--                     (EApply 
--                         (EApply (EVar "filter")
--                             (ELambda ("x",TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0)))
--                         ) 
--                         (EApply (EApply (EVar "::@int") (EIntLit 100))
--                             (EApply (EApply (EVar "::@int") (ESub (EIntLit 0) (EIntLit 98)))
--                                 (EApply (EApply (EVar "::@int") (EIntLit 43))
--                                     (EApply (EApply (EVar "::@int") (ESub (EIntLit 0) (EIntLit 5)))
--                                         (EApply (EApply (EVar "::@int") (EIntLit 23))
--                                             (EApply (EApply (EVar "::@int") (EIntLit 8))
--                                                 (EVar "[]@int")
--                                             )
--                                         )
--                                     )
--                                 )
--                             )
--                         )
--                     )
--             )
--         )
--     )

-- prog3 =
--     Program [] $
--     makeFun ("foo", TInt) [("x", TInt), ("y", TInt)]
--     (
--       EAdd (EVar "x") (EVar "y")
--     ) $
--     makeFun ("inner", TInt) [("x", TArrow TInt TInt), ("y", TArrow TInt TInt)]
--     (
--       EAdd (EApply (EVar "x") (EIntLit 13)) (EApply (EVar "y") (EIntLit 17))
--     ) $
--     callFun (EVar "inner") [(EApply (EVar "foo") (EIntLit 9)), (EApply (EVar "foo") (EIntLit 2))]

-- getFromMaybe :: Maybe EvalType.Context -> String
-- getFromMaybe c =
--     case c of
--         Nothing -> "Nothing"
--         Just x -> logs x

-- main :: IO ()
-- main = do
--     putStrLn ""
--     putStrLn $ show $ EvalType.evalType prog2
--     putStrLn $ getFromMaybe $ EvalType.getlogs prog2

main :: IO()
main = do
    putStrLn ">> Usage: Input \"parser\" and press enter to enter parser mode. Or input \"repl\" to enter REPL mode"
    hFlush stdout
    cmd <- getLine
    case cmd of
        "parser" -> do
            putStrLn "Entering parser mode. Please input expression to parser and press Ctrl-D"
            hFlush stdout
            parser
        "repl" -> do
            putStrLn "Entering REPL mode."
            hFlush stdout
            LambdaParser.loop []
