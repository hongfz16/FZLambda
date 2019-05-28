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

generateConstructorTArrow :: String -> String -> [Type] -> Type
generateConstructorTArrow adtname conname (ftype:types) = TArrow ftype (generateConstructorTArrow adtname conname types)
generateConstructorTArrow adtname conname [] = TData adtname

findAdtByConstr :: String -> [ADT] -> ContextState String
findAdtByConstr conname ((ADT name cases):adts) = case isInCases conname cases of
                                                    True -> return name
                                                    False -> findAdtByConstr conname adts
                                                  where isInCases tarname ((con, _):cases)
                                                          | tarname == con = True
                                                          | otherwise = isInCases tarname cases
                                                        isInCases tarname [] = False
findAdtByConstr conname [] = lift Nothing

findArgsByConstr :: String -> [ADT] -> ContextState [Type]
findArgsByConstr conname ((ADT name cases):adts) = case isInCases conname cases of
                                                     True -> getTypes conname cases
                                                     False -> findArgsByConstr conname adts
                                                   where isInCases tarname ((con, _):cases)
                                                           | tarname == con = True
                                                           | otherwise = isInCases tarname cases
                                                         isInCases tarname [] = False
                                                         getTypes tarname ((con, types):cases)
                                                           | tarname == con = return types
                                                           | otherwise = getTypes tarname cases
                                                         getTypes tarname [] = lift Nothing
findArgsByConstr conname [] = lift Nothing

checkPatternArgs :: [Type] -> [Pattern] -> ContextState Bool
checkPatternArgs (t:ts) (p:ps) = do
  case p of
    PVar x -> checkPatternArgs ts ps
    _ -> do
      tp <- getPatternType p
      if t == tp then (checkPatternArgs ts ps) else return False
checkPatternArgs [] [] = return True

getPatternType :: Pattern -> ContextState Type -- other than PVar
getPatternType p = case p of
  PBoolLit b -> return TBool
  PIntLit i -> return TInt
  PCharLit c -> return TChar
  PData name ps -> do
    context <- get
    adtname <- findAdtByConstr name (adts context)
    args <- findArgsByConstr name (adts context)
    valid <- checkPatternArgs args ps
    case valid of
      True -> return $ TData adtname
      False -> lift Nothing
    -- return $ TData adtname

validatePatternType :: Type -> [(Pattern, Expr)] -> ContextState Type -- validate [p1, p2, ...] and e0 have the same type
validatePatternType t0 ((p1, e1):cases) = do
  tcases <- validatePatternType t0 cases
  case p1 of
    PVar v -> return t0
    _ -> do
      tp1 <- getPatternType p1
      if tp1 == t0 then return t0 else lift Nothing
validatePatternType t0 [] = return t0

patternContextEval :: String -> [Pattern] -> Expr -> ContextState Type
patternContextEval consName ps e = do
  context <- get
  args <- findArgsByConstr consName (adts context)
  innerEval args ps e
  where innerEval (t:ts) (p:ps) e = case p of
                                      PVar x -> do
                                        context <- get
                                        newcontext <- return $ addBinding (x, t) context
                                        put newcontext
                                        te <- innerEval ts ps e
                                        aftercontext <- get
                                        newaftercontext <- return $ popBinding aftercontext
                                        put newaftercontext
                                        return te
                                      PData iconsName ips -> case t of
                                        TData adtName -> do
                                          context <- get
                                          args <- findArgsByConstr iconsName (adts context)
                                          innerEval (args ++ ts) (ips ++ ps) e
                                      _ -> innerEval ts ps e
        innerEval [] [] e = eval e

isSameCaseReturnType :: Type -> Type -> [(Pattern, Expr)] -> ContextState Bool
isSameCaseReturnType targettype pt ((p1, e1):cases) = do
  case p1 of
    PVar x -> do
      context <- get
      newcontext <- return (addBinding (x, pt) context)
      put newcontext
      t1 <- eval e1
      aftercontext <- get
      newaftercontext <- return (popBinding aftercontext)
      put newaftercontext
      if t1 == targettype then isSameCaseReturnType targettype pt cases else return False
    PData s ts -> do
      t1 <- patternContextEval s ts e1
      if t1 == targettype then isSameCaseReturnType targettype pt cases else return False
    _ -> do
      t1 <- eval e1
      if t1 == targettype then isSameCaseReturnType targettype pt cases else return False
isSameCaseReturnType targettype pt [] = return True

validateCaseReturnType :: Type -> [(Pattern, Expr)] -> ContextState Type -- pattern type -> cases -> ... / validate e1, e2, have same type and return it
validateCaseReturnType pt ((p1, e1):cases) = do
  case p1 of
    PVar x -> do
      context <- get
      newcontext <- return (addBinding (x, pt) context)
      put newcontext
      t1 <- eval e1
      aftercontext <- get
      newaftercontext <- return (popBinding aftercontext)
      put newaftercontext
      issame <- isSameCaseReturnType t1 pt cases
      case issame of
        True -> return t1
        False -> lift Nothing
    PData s ts -> do
      t1 <- patternContextEval s ts e1
      issame <- isSameCaseReturnType t1 pt cases
      case issame of
        True -> return t1
        False -> lift Nothing
    _ -> do
      t1 <- eval e1
      issame <- isSameCaseReturnType t1 pt cases
      case issame of
        True -> return t1
        False -> lift Nothing
validateCaseReturnType pt [] = lift Nothing

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
eval (ECase e0 cases) = do
  t0 <- eval e0
  pt <- validatePatternType t0 cases
  rt <- validateCaseReturnType pt cases
  return rt
-- ... more
-- eval _ = undefined

getAllADT :: [ADT] -> [(String, Type)]
getAllADT ((ADT name constructors):adts) = (getConstructors name constructors) ++ getAllADT adts
                                            where getConstructors name ((conname, types):cons) = (conname, generateConstructorTArrow name conname types):(getConstructors name cons)
                                                  getConstructors name [] = []
getAllADT [] = []

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { bindings = getAllADT adts
           ,adts=adts } -- 可以用某种方式定义上下文，用于记录变量绑定状态

-- evalType :: Program -> Maybe Type
-- evalType p = do
--   t <- preEvalType p
--   case t of
--     TSpecData adtname conname -> Just (TData adtname)
--     _ -> Just t
