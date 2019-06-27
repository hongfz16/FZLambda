-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VLambda (String, Expr, Value)
  | VLambdaOuter (String, Value, Value)
  | VData (String, String, [Value], [Type])
  | NullValue
  -- ... more
  deriving (Show, Eq)

data Context = Context { bindings :: [(String, Value)]
                        ,exprBindings :: [(String, Expr)]
                        ,pushOrder :: [String]
                        ,adts :: [(String, String, [Type])] -- constructor name, adt name, types
                         -- 可以用某种方式定义上下文，用于记录变量绑定状态
                       } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

findVarTopType :: String -> Context -> ContextState String
findVarTopType target context = innerFind target (pushOrder context) (bindings context) (exprBindings context)
                                where innerFind target (ft:orders) ((bs, bv):binds) ((es, ee):exprs)
                                          | ft == "value" = if target == bs then return "value" else innerFind target orders binds ((es, ee):exprs)
                                          | ft == "expr" = if target == es then return "expr" else innerFind target orders ((bs, bv):binds) exprs
                                      innerFind target (ft:orders) ((bs, bv):binds) []
                                          | ft == "value" = if target == bs then return "value" else innerFind target orders binds []
                                          | ft == "expr" = lift Nothing
                                      innerFind target (ft:orders) [] ((es, ee):exprs)
                                          | ft == "value" = lift Nothing
                                          | ft == "expr" = if target  == es then return "expr" else innerFind target orders [] exprs
                                      innerFind target [] [] [] = return "adt"

addBinding :: (String, Value) -> Context -> Context
addBinding (s, v) c = Context { bindings = (s, v):(bindings c)
                               ,exprBindings = (exprBindings c)
                               ,pushOrder = "value":(pushOrder c)
                               ,adts = (adts c)}

findBinding :: String -> Context -> ContextState Value
findBinding s c = findList s $ bindings c
                  where findList s ((n, t):bs) | s == n = return t
                                               | otherwise = findList s bs
                        findList s [] = return NullValue

popBinding :: Context -> Context
popBinding c = Context { bindings = tail $ bindings c
                        ,exprBindings = (exprBindings c)
                        ,pushOrder = tail $ pushOrder c
                        ,adts = (adts c)}

addExpr :: (String, Expr) -> Context -> Context
addExpr (s, e) c = Context { bindings = (bindings c)
                            ,exprBindings = (s, e):(exprBindings c)
                            ,pushOrder = "expr":(pushOrder c)
                            ,adts = (adts c)}

findExpr :: String -> Context -> ContextState Expr
findExpr s c = findList s $ exprBindings c
               where findList s ((n, e):bs) | s == n = return e
                                            | otherwise = findList s bs
                     findList s [] = lift Nothing

popExpr :: Context -> Context
popExpr c = Context { bindings = (bindings c)
                     ,exprBindings = tail $ exprBindings c
                     ,pushOrder = tail $ pushOrder c
                     ,adts = (adts c)}

findAdtList :: String -> [(String, String, [Type])] -> ContextState Value
findAdtList s (a:as) = case a of
  (consName, adtName, types) -> if s == consName then return (VData (consName, adtName, [], types)) else findAdtList s as
findAdtList s [] = lift Nothing


findAdt :: String -> Context -> ContextState Value
findAdt s c = findAdtList s (adts c)

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case ev of
    VInt i -> return i
    _ -> lift Nothing

getChar :: Expr -> ContextState Char
getChar e = do
  ev <- eval e
  case ev of
    VChar c -> return c
    _ -> lift Nothing

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c
eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd el er) = do
  vl <- getBool el
  case vl of
    False -> return (VBool False)
    True -> do
      vr <- getBool er
      return (VBool $ vl && vr)
eval (EOr el er) = do
  vl <- getBool el
  case vl of
    True -> return (VBool True)
    False -> do
      vr <- getBool er
      return (VBool $ vl || vr)
eval (EAdd el er) = do
  vl <- getInt el
  vr <- getInt er
  return (VInt $ vl + vr)
eval (ESub el er) = do
  vl <- getInt el
  vr <- getInt er
  return (VInt $ vl - vr)
eval (EMul el er) = do
  vl <- getInt el
  vr <- getInt er
  return (VInt $ vl * vr)
eval (EDiv el er) = do
  vl <- getInt el
  vr <- getInt er
  case vr of
    0 -> lift Nothing
    _ -> return (VInt $ vl `div` vr)
eval (EMod el er) = do
  vl <- getInt el
  vr <- getInt er
  case vr of
    0 -> lift Nothing
    _ -> return (VInt $ vl `mod` vr)
eval (EEq el er) = do
  vl <- eval el
  vr <- eval er
  case (vl, vr) of
    (VBool bl, VBool br) -> return (VBool $ vl == vr)
    (VChar cl, VChar cr) -> return (VBool $ cl == cr)
    (VInt il, VInt ir) -> return (VBool $ il == ir)
    _ -> lift Nothing
eval (ENeq el er) = do
  vl <- eval el
  vr <- eval er
  case (vl, vr) of
    (VBool bl, VBool br) -> return (VBool $ vl /= vr)
    (VChar cl, VChar cr) -> return (VBool $ cl /= cr)
    (VInt il, VInt ir) -> return (VBool $ il /= ir)
    _ -> lift Nothing
eval (ELt el er) = do
  vl <- eval el
  vr <- eval er
  case (vl, vr) of
    (VChar cl, VChar cr) -> return (VBool $ cl < cr)
    (VInt il, VInt ir) -> return (VBool $ il < ir)
    _ -> lift Nothing
eval (EGt el er) = do
  vl <- eval el
  vr <- eval er
  case (vl, vr) of
    (VChar cl, VChar cr) -> return (VBool $ cl > cr)
    (VInt il, VInt ir) -> return (VBool $ il > ir)
    _ -> lift Nothing
eval (ELe el er) = do
  vl <- eval el
  vr <- eval er
  case (vl, vr) of
    (VChar cl, VChar cr) -> return (VBool $ cl <= cr)
    (VInt il, VInt ir) -> return (VBool $ il <= ir)
    _ -> lift Nothing
eval (EGe el er) = do
  vl <- eval el
  vr <- eval er
  case (vl, vr) of
    (VChar cl, VChar cr) -> return (VBool $ cl >= cr)
    (VInt il, VInt ir) -> return (VBool $ il >= ir)
    _ -> lift Nothing
eval (EIf ec et ee) = do
  vc <- eval ec
  case vc of
    (VBool True) -> eval et
    (VBool False) -> eval ee
    _ -> lift Nothing
eval (ELambda (pn, pt) e) = do
  case e of
    (ELambda (en, et) ee) -> do
      ve <- eval e
      return (VLambdaOuter (pn, ve, NullValue))
    _ -> return (VLambda (pn, e, NullValue))
eval (ELet (n, e1) e2) = do
  context <- get
  newcontext <- return (addExpr (n, e1) context)
  put newcontext
  v2 <- eval e2
  aftercontext <- get
  newaftercontext <- return (popExpr aftercontext)
  put newaftercontext
  return v2
  -- v1 <- eval e1
  -- context <- get
  -- newcontext <- return (addBinding (n, v1) context)
  -- put newcontext
  -- v2 <- eval e2
  -- aftercontext <- get
  -- newaftercontext <- return (popBinding aftercontext)
  -- put newaftercontext
  -- return v2
eval (ELetRec f (x, tx) (e1, ty) e2) = do
  case e1 of
    (ELambda (e1n, e1t) e1e) -> do
      ve1 <- eval e1
      context <- get
      newcontext <- return (addBinding (f, (VLambdaOuter (x, ve1, NullValue))) context)
      put newcontext
      v2 <- eval e2
      aftercontext <- get
      newaftercontext <- return (popBinding aftercontext)
      put newaftercontext
      return v2
    _ -> do
      context <- get
      newcontext <- return (addBinding (f, (VLambda (x, e1, NullValue))) context)
      put newcontext
      v2 <- eval e2
      aftercontext <- get
      newaftercontext <- return (popBinding aftercontext)
      put newaftercontext
      return v2
eval (EVar n) = do
  context <- get
  strtype <- findVarTopType n context
  case strtype of
    "expr" -> do
      e <- findExpr n context
      ve <- eval e
      return ve
    "value" -> do
      v <- findBinding n context
      return v
    "adt" -> do
      v <- findAdt n context
      return v
eval (EApply e1 e2) = do -- TODO: 这里的实现需要先求出e2，这样不符合惰性求值，因此需要改一改
  v2 <- eval e2
  v1 <- eval e1
  case v1 of
    (VLambda (ln, le, lv)) -> do
      context <- get
      newcontext <- return (addBinding (ln, v2) context)
      put newcontext
      vle <- eval le
      aftercontext <- get
      newaftercontext <- return (popBinding aftercontext)
      put newaftercontext
      return vle
    (VLambdaOuter (ln, lev, lv)) -> do
      tempv <- addLambdaBinding v1 v2
      case (checkLambda tempv) of
        False -> return tempv
        True -> do
          evaltempv <- evalLambda tempv
          return evaltempv
    (VData (consName, adtName, params, types)) -> do
      vdata <- addParamsToAdt (VData (consName, adtName, params, types)) v2
      return vdata
eval (ECase e0 cases) = do
  v0 <- eval e0
  matched <- matchCase v0 cases
  case matched of
    (PVar s, e1) -> do
      context <- get
      newcontext <- return (addBinding (s, v0) context)
      put newcontext
      v1 <- eval e1
      aftercontext <- get
      newaftercontext <- return (popBinding aftercontext)
      put newaftercontext
      return v1
    (PData consName ps, e1) -> case v0 of
      (VData (_, _, vs, _)) -> do
        evalCase vs ps e1
      _ -> lift Nothing
    (_, e1) -> eval e1
-- ... more
-- eval _ = undefined

evalCase :: [Value] -> [Pattern] -> Expr -> ContextState Value
evalCase (v:vs) (p:ps) e = case p of
  (PVar s) -> do
    context <- get
    newcontext <- return (addBinding (s, v) context)
    put newcontext
    result <- evalCase vs ps e
    aftercontext <- get
    newaftercontext <- return (popBinding aftercontext)
    put newaftercontext
    return result
  (PData consName ips) -> case v of
    (VData (consName, adtName, ivs, [])) -> evalCase (ivs ++ vs) (ips ++ ps) e
    _ -> lift Nothing
  _ -> evalCase vs ps e
evalCase [] [] e = eval e
evalCase [] (p:ps) e = lift Nothing
evalCase (v:vs) [] e = lift Nothing

addLambdaBinding :: Value -> Value -> ContextState Value
addLambdaBinding (VLambdaOuter (ln, lev, lv)) v = do
  case lv of
    NullValue -> return (VLambdaOuter (ln, lev, v))
    _ -> do
      inner <- addLambdaBinding lev v
      return (VLambdaOuter (ln, inner, lv))
addLambdaBinding (VLambda (ln, le, lv)) v = do
  case lv of
    NullValue -> return (VLambda (ln, le, v))
    _ -> lift Nothing

evalLambda :: Value -> ContextState Value
evalLambda (VLambdaOuter (evalln, evallev, evallv)) = do
  context <- get
  newcontext <- return (addBinding (evalln, evallv) context)
  put newcontext
  result <- evalLambda evallev
  aftercontext <- get
  newaftercontext <- return (popBinding aftercontext)
  put newaftercontext
  return result
evalLambda (VLambda (evalln, evalle, evallv)) = do
  context <-get
  newcontext <- return (addBinding (evalln, evallv) context)
  put newcontext
  result <- eval evalle
  aftercontext <- get
  newaftercontext <- return (popBinding aftercontext)
  put newaftercontext
  return result

checkLambda :: Value -> Bool
checkLambda (VLambdaOuter (n, ev, v)) =
  case v of
    NullValue -> False
    _ -> checkLambda ev
checkLambda (VLambda (n, e, v)) =
  case v of
    NullValue -> False
    _ -> True

matchParams :: [Pattern] -> [Value] -> Bool
matchParams (p:ps) (v:vs) = case p of
  (PBoolLit bp) -> case v of
                    (VBool bv) -> if bp == bv then matchParams ps vs else False
                    _ -> False
  (PIntLit ip) -> case v of
                    (VInt iv) -> if ip == iv then matchParams ps vs else False
                    _ -> False
  (PCharLit cp) -> case v of
                    (VChar cv) -> if cp == cv then matchParams ps vs else False
                    _ -> False
  (PVar s) -> matchParams ps vs
  (PData consName ips) -> case v of
                    (VData (vconsName, adtName, params, [])) -> if consName == vconsName then (if matchParams ips params then matchParams ps vs else False) else False
                    _ -> False
matchParams [] [] = True
matchParams [] (v:vs) = False
matchParams (p:ps) [] = False

matchCase :: Value -> [(Pattern, Expr)] -> ContextState (Pattern, Expr)
matchCase v0 ((p1, e1):cases) = case p1 of
  (PBoolLit bp) -> case v0 of
                    (VBool bv) -> if bp == bv then return (p1, e1) else matchCase v0 cases
                    _ -> matchCase v0 cases
  (PIntLit ip) -> case v0 of
                    (VInt iv) -> if ip == iv then return (p1, e1) else matchCase v0 cases
                    _ -> matchCase v0 cases
  (PCharLit cp) -> case v0 of
                    (VChar cv) -> if cp == cv then return (p1, e1) else matchCase v0 cases
                    _ -> matchCase v0 cases
  (PVar s) -> return (p1, e1)
  (PData consName ps) -> case v0 of
                    (VData (vconsName, adtName, params, [])) -> if consName == vconsName then (if matchParams ps params then return (p1, e1) else matchCase v0 cases) else matchCase v0 cases
                    _ -> matchCase v0 cases
matchCase v0 [] = lift Nothing

addParamsToAdt :: Value -> Value -> ContextState Value
addParamsToAdt (VData (consName, adtName, params, (t:ts))) v = return $ VData (consName, adtName, (params ++ [v]), ts)
addParamsToAdt (VData (consName, adtName, _, [])) v = lift Nothing

constructAdt :: [ADT] -> [(String, String, [Type])]
constructAdt ((ADT adtName cases):as) = (constructCases adtName cases) ++ (constructAdt as)
                                            where constructCases adtName ((consName, types):cases) = (consName, adtName, types):(constructCases adtName cases)
                                                  constructCases adtName [] = []
constructAdt [] = []

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { bindings=[]
           ,exprBindings=[]
           ,pushOrder=[]
           ,adts=constructAdt adts } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  Just (VData (s, _, vs, _)) -> RData $ s ++ " " ++ (show vs)
  -- Just (VLambdaOuter (s, ev, v)) -> RInt 10000
  -- Just (VLambda (s, e, v)) -> RInt 10001
  -- Just (NullValue) -> RInt 10002
  _ -> RInvalid
