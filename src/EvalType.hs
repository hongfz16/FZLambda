-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State

data Context = Context { bindings :: [(String, Type)] -- 可以用某种方式定义上下文，用于记录变量绑定状态
                        ,adts :: [ADT]}
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

addBinding :: (String, Type) -> Context -> Context
addBinding (s, t) c = Context { bindings = (s, t):(bindings c), adts = adts c}

popBinding :: Context -> Context
popBinding c = Context { bindings = tail $ bindings c, adts = adts c }

findBinding :: String -> Context -> ContextState Type
findBinding s c = findList s $ bindings c
                  where findList s ((n, t):bs) | s == n = return t
                                               | otherwise = findList s bs
                        findList s [] = lift Nothing

findADT :: String -> Context -> ContextState ADT
findADT s c = findList s $ adts c
              where findList s ((ADT name ls):adts) | name == s = return (ADT name ls)
                                                    | otherwise = findList s adts
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

generateConstructorTArrow :: String -> [Type] -> Type
generateConstructorTArrow adtname (ftype:types) = TArrow ftype (generateConstructorTArrow adtname types)
generateConstructorTArrow adtname [] = TData adtname

getPatternType :: Pattern -> ContextState Type -- other than PVar
getPatternType p = case p of
  PBoolLit b -> return TBool
  PIntLit i -> return TInt
  PCharLit c -> return TChar
  PData name ps -> return $ TData name

validatePatternType :: Type -> [(Pattern, Expr)] -> ContextState Type -- validate [p1, p2, ...] and e0 have the same type
validatePatternType t0 ((p1, e1):cases) = do
  tcases <- validatePatternType t0 cases
  case p1 of
    PVar v -> return t0
    _ -> do
      tp1 <- getPatternType p1
      if tp1 == t0 then return t0 else lift Nothing
validatePatternType t0 [] = return t0

validateCaseReturnType :: Type -> [(Pattern, Expr)] -> ContextState Type -- pattern type -> cases -> ... / validate e1, e2, have same type and return it
validateCaseReturnType pt cases = undefined

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
  aftercontext <- get
  newaftercontext <- return (popBinding aftercontext)
  put newaftercontext
  return (TArrow pt te)
eval (ELet (n, e1) e2) = do
  t1 <- eval e1
  context <- get
  newcontext <- return (addBinding (n, t1) context)
  put newcontext
  t2 <- eval e2
  aftercontext <- get
  newaftercontext <- return (popBinding aftercontext)
  put newaftercontext
  return t2
eval (ELetRec f (x, tx) (e1, ty) e2) = do
  context <- get
  newcontext <- return (addBinding (f, TArrow tx ty) context)
  newcontext' <- return (addBinding (x, tx) newcontext)
  put newcontext'
  t1 <- eval e1
  aftercontext <- get
  newaftercontext <- return (popBinding aftercontext)
  put newaftercontext
  t2 <- eval e2
  aftercontext' <- get
  newaftercontext' <- return (popBinding aftercontext')
  put newaftercontext'
  if t1 == ty then return t2 else lift Nothing
eval (EVar n) = do
  context <- get
  t <- findBinding n context
  return t
eval (EApply e1 e2) = do
  t1 <- eval e1
  t2 <- eval e2
  case t1 of
    TArrow t10 t11 -> if t10 == t2 then return t11 else lift Nothing 
    _ -> lift Nothing
eval (ECase e0 cases) = undefined
-- ... more
-- eval _ = undefined

getAllADT :: [ADT] -> [(String, Type)]
getAllADT ((ADT name constructors):adts) = (getConstructors name constructors) ++ getAllADT adts
                                            where getConstructors name ((conname, types):cons) = (conname, generateConstructorTArrow name types):(getConstructors name cons)
                                                  getConstructors name [] = []
getAllADT [] = []

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { bindings = getAllADT adts
           ,adts=adts } -- 可以用某种方式定义上下文，用于记录变量绑定状态
