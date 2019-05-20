-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VLambda (String, Expr)
  -- ... more
  deriving (Show, Eq)

data Context = Context { bindings::[(String, Value)],
                         stack::[(String, Value)]
                         -- 可以用某种方式定义上下文，用于记录变量绑定状态
                       } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

removeOldBinding :: String -> Context -> Context
removeOldBinding s c = Context { bindings = removeList s $ bindings c
                                            where removeList s ((n, t):bs) | s == n = removeList s bs
                                                                           | otherwise = (n, t):(removeList s bs),
                                 stack = stack c
                               }

addBinding :: (String, Value) -> Context -> Context
addBinding (s, v) c = Context { bindings = (s, v):(bindings $ removeOldBinding s c),
                                stack = stack c
                              }

findBinding :: String -> Context -> ContextState Value
findBinding s c = findList s $ bindings c
                  where findList s ((n, t):bs) | s == n = return t
                                               | otherwise = findList s bs
                        findList s [] = lift Nothing

-- pushStack :: (String, Value) -> Context -> Context
-- pushStack (s, v) c = 

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
  return (VLambda (pn, e))
eval (ELet (n, e1) e2) = do
  v1 <- eval e1
  context <- get
  newcontext <- return (addBinding (n, v1) context)
  put newcontext
  v2 <- eval e2
  return v2
eval (ELetRec f (x, tx) (e1, ty) e2) = do
  context <- get
  newcontext <- return (addBinding (f, (VLambda (x, e1))) context)
  put newcontext
  v2 <- eval e2
  return v2
eval (EVar n) = do
  context <- get
  v <- findBinding n context
  return v
eval (EApply e1 e2) = do
  vlamb <- eval e1
  case vlamb of
    (VLambda (nl, el)) -> do
      v2 <- eval e2
      context <- get
      newcontext <- return (addBinding (nl, v2) context)
      put newcontext
      vl <- eval el
      return vl
    _ -> lift Nothing
-- ... more
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { bindings=[] } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
