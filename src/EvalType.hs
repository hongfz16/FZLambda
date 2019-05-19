-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State

data Context = Context { bindings :: [(String, Type)] -- 可以用某种方式定义上下文，用于记录变量绑定状态
                       }
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

addBinding :: (String, Type) -> Context -> Context
addBinding (s, t) c = Context { bindings = (s, t):(bindings c)}

findBinding :: String -> Context -> ContextState Type
findBinding s c = findList s $ bindings c
                  where findList s ((n, t):bs) | s == n = return t
                                               | otherwise = findList s bs
                        findList s [] = lift Nothing

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

isSameEqType :: Expr -> Expr -> ContextState Type
isSameEqType el er = do
  elt <- eval el
  ert <- eval er
  case (elt, ert) of
    (TBool, TBool) -> return TBool
    (TInt, TInt) -> return TInt
    (TChar, TChar) -> return TChar
    (_, _) -> lift Nothing

isSameComType :: Expr -> Expr -> ContextState Type
isSameComType el er = do
  elt <- eval el
  ert <- eval er
  case (elt, ert) of
    (TInt, TInt) -> return TInt
    (TChar, TChar) -> return TChar
    (_, _) -> lift Nothing

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar
eval (ENot e) = isBool e >> return TBool
eval (EAnd el er) = isBool el >> isBool er >> return TBool
eval (EOr el er) = isBool el >> isBool er >> return TBool
eval (EAdd el er) = isInt el >> isInt er >> return TInt
eval (ESub el er) = isInt el >> isInt er >> return TInt
eval (EMul el er) = isInt el >> isInt er >> return TInt
eval (EDiv el er) = isInt el >> isInt er >> return TInt
eval (EMod el er) = isInt el >> isInt er >> return TInt
eval (EEq el er) = isSameEqType el er >> return TBool
eval (ENeq el er) = isSameEqType el er >> return TBool
eval (ELt el er) = isSameComType el er >> return TBool
eval (EGt el er) = isSameComType el er >> return TBool
eval (ELe el er) = isSameComType el er >> return TBool
eval (EGe el er) = isSameComType el er >> return TBool
eval (EIf ec et ee) = do
  tc <- isBool ec
  tte <- isSameEqType et ee
  case (tc, tte) of
    (TBool, TBool) -> return TBool
    (TBool, TInt) -> return TInt
    (TBool, TChar) -> return TChar
eval (ELambda (pn, pt) e) = do
  context <- get
  newcontext <- return (addBinding (pn, pt) context)
  put newcontext
  te <- eval e
  return (TArrow pt te)
eval (EVar n) = do
  context <- get
  t <- findBinding n context
  return t
  -- ... more
eval _ = undefined


evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { bindings=[] } -- 可以用某种方式定义上下文，用于记录变量绑定状态
