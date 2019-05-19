-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  -- ... more
  deriving (Show, Eq)

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
                          } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

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
-- ... more
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
