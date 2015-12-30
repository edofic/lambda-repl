module Eval where

import AST
import Control.Monad.Except
import Types
import qualified Data.Map as Map
import Control.Monad.State


maybeToEval :: String -> Maybe a -> LambdaMonad a
maybeToEval _ (Just a) = return a
maybeToEval name Nothing = throwError (NotFound name)

local :: MonadState s m => s -> m a -> m a
local s ma = do
  original <- get
  put s
  res <- ma
  put original
  return res

eval :: Expr -> LambdaMonad Value
eval (Value v) = return $ VInt v
eval (Ident ident) = get >>= (maybeToEval ident . Map.lookup ident)
eval (Lambda name expr ) = VFunc name expr <$> get
eval (Let name expr) = do
  v <- eval expr
  modify $ Map.insert name v
  return v
eval (Application f arg) =
  let evalFuncVal x (VFunc name expr scope') = local (Map.insert name x scope') $ eval expr
      evalFuncVal x (VDelayed v)             = lift v >>=  evalFuncVal x
      evalFuncVal _ othr                     =
        throwError $ TypeError $ show othr ++ " is not a function"
  in do
    scope <- get
    f' <- eval f
    let x = VDelayed $ evalStateT (eval arg) scope
    evalFuncVal x f'

