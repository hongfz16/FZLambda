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
  | NullValue
  -- ... more
  deriving (Show, Eq)

data Context = Context { bindings::[(String, Value)]
                         -- 可以用某种方式定义上下文，用于记录变量绑定状态
                       } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

addBinding :: (String, Value) -> Context -> Context
addBinding (s, v) c = Context { bindings = (s, v):(bindings c)
                              }

findBinding :: String -> Context -> ContextState Value
findBinding s c = findList s $ bindings c
                  where findList s ((n, t):bs) | s == n = return t
                                               | otherwise = findList s bs
                        findList s [] = lift Nothing

popBinding :: Context -> Context
popBinding c = Context { bindings = tail $ bindings c}

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
  v1 <- eval e1
  context <- get
  newcontext <- return (addBinding (n, v1) context)
  put newcontext
  v2 <- eval e2
  aftercontext <- get
  newaftercontext <- return (popBinding aftercontext)
  put newaftercontext
  return v2
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
  v <- findBinding n context
  return v
eval (EApply e1 e2) = do
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
-- ... more
eval _ = undefined

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

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { bindings=[] } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  -- Just (VLambdaOuter (s, ev, v)) -> RInt 10000
  -- Just (VLambda (s, e, v)) -> RInt 10001
  -- Just (NullValue) -> RInt 10002
  _ -> RInvalid
