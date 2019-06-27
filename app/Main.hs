module Main where

import System.IO
import EvalType
import AST
import EvalValue
import LambdaParser

intlist = ADT "[int]" [("[]@int", []), ("::@int", [TInt,TData "[int]"])]
maybeint = ADT "maybeint" [("Just", [TInt]), ("Nothing", [])]
adt = [maybeint, intlist]

safediv = ELambda ("x", TInt) $
    ELambda ("y", TInt) $
        EIf (EEq (EVar "y") (EIntLit 0)) (EVar "Nothing") (EApply (EVar "Just") (EDiv (EVar "x") (EVar "y")))

_map = ELetRec "map" ("f", TArrow TInt TInt)
        (   ELambda ("xs", TData "[int]")
            (ECase (EVar "xs") [
                (PData "[]@int" [], EVar "[]@int"),
                (PData "::@int" [PVar "x", PVar "xs'"], EApply (EApply (EVar "::@int") (EApply (EVar "f") (EVar "x"))) (EApply (EApply (EVar "map") (EVar "f")) (EVar "xs'")))
            ])
        ,
            TArrow (TData "[int]") (TData "[int]")
        )

_filter = ELetRec "filter" ("p", TArrow TInt TBool) 
            (   ELambda ("xs", TData "[int]")
                (ECase (EVar "xs") [
                    (PData "[]@int" [], EVar "[]@int"),
                    (PData "::@int" [PVar "x", PVar "xs'"], EIf (EApply (EVar "p") (EVar "x")) (EApply (EApply (EVar "::@int") (EVar "x")) (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs'"))) (EApply (EApply (EVar "filter") (EVar "p")) (EVar "xs'")))
                ])
            ,
                TArrow (TData "[int]") (TData "[int]")
            )

listeq = ELetRec "listeq" ("xs", TData "[int]")
            (   ELambda ("ys", TData "[int]")
                (ECase (EVar "xs") [
                    (PData "[]@int" [],
                        ECase (EVar "ys") [
                            (PData "[]@int" [], EBoolLit True),
                            (PData "::@int" [PVar "y", PVar "ys'"], EBoolLit False)
                        ]
                    ),
                    (PData "::@int" [PVar "x", PVar "xs'"],
                        ECase (EVar "ys") [
                            (PData "[]@int" [], EBoolLit False),
                            (PData "::@int" [PVar "y", PVar "ys'"], EAnd (EEq (EVar "x") (EVar "y")) (EApply (EApply (EVar "listeq") (EVar "xs'")) (EVar "ys'")))
                        ]
                    )
                ])
            ,
                TArrow (TData "[int]") TBool
            )

list12 = EApply (EApply (EVar "::@int") (EIntLit 1))
            (EApply (EApply (EVar "::@int") (EIntLit 2))
                (EVar "[]@int")
            )

list13 = EApply (EApply (EVar "::@int") (EIntLit 1))
            (EApply (EApply (EVar "::@int") (EIntLit 3))
                (EVar "[]@int")
            )

list1 = EApply (EApply (EVar "::@int") (EIntLit 1)) (EVar "[]@int")

list123 = EApply (EApply (EVar "::@int") (EIntLit 1))
            (EApply (EApply (EVar "::@int") (EIntLit 2))
                (EApply (EApply (EVar "::@int") (EIntLit 3))
                    (EVar "[]@int")
                )
            )

prog1 = Program adt $
    ELet ("safediv", safediv) $
        EApply (EApply (EVar "safediv") (EIntLit 7)) (EIntLit 2)

prog2 = Program adt $
    ELet ("safediv", safediv) $
        EApply (EApply (EVar "safediv") (EIntLit 7)) (EIntLit 0)

prog3 = Program adt $
    listeq $
        EApply (EApply (EVar "listeq") list12) list12

prog4 = Program adt $
    listeq $
        EApply (EApply (EVar "listeq") list12) list13

prog5 = Program adt $
    listeq $
        EApply (EApply (EVar "listeq") list12) list1

prog6 = Program adt $
    _map $
        EApply (EApply (EVar "map")
                    (ELambda ("x", TInt) (EMul (EVar "x") (EVar "x")))
                )
        list123

prog7 = Program adt $
    _filter $
        EApply (EApply (EVar "filter") 
                    (ELambda ("x", TInt)
                        (EIf (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))
                            (EBoolLit True) (EBoolLit False)
                        )
                    )
                )
        list123

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
        "adttest" -> do
            putStrLn "> Testing Program 01"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog1
            putStrLn $ show $ EvalValue.evalValue prog1
            putStrLn "> Testing Program 02"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog2
            putStrLn $ show $ EvalValue.evalValue prog2
            putStrLn "> Testing Program 03"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog3
            putStrLn $ show $ EvalValue.evalValue prog3
            putStrLn "> Testing Program 04"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog4
            putStrLn $ show $ EvalValue.evalValue prog4
            putStrLn "> Testing Program 05"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog5
            putStrLn $ show $ EvalValue.evalValue prog5
            putStrLn "> Testing Program 06"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog6
            putStrLn $ show $ EvalValue.evalValue prog6
            putStrLn "> Testing Program 07"
            hFlush stdout
            putStrLn $ show $ EvalType.evalType prog7
            putStrLn $ show $ EvalValue.evalValue prog7
        _ -> do
            putStrLn "Wrong command!"
            hFlush stdout
            main