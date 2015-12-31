module Eval where

import AST
import Control.Monad.Except
import Types
import qualified Data.Map as Map
import Control.Monad.State

eval :: Expr -> LambdaMonad Value
eval (Value v) = return $ VInt v
eval (Ident ident) = do
  scope <- get
  case Map.lookup ident scope of Just a  -> return a
                                 Nothing -> throwError $ NotFound ident
eval (Lambda name expr ) = VFunc name expr <$> get
eval (Let name expr) = do
  v <- eval expr
  modify $ Map.insert name v
  return v
eval (Application f arg) = do
    scope <- get
    f' <- eval f
    let x = VDelayed $ evalStateT (eval arg) scope
    evalFuncVal x f'

evalFuncVal :: Value -> Value -> LambdaMonad Value
evalFuncVal x (VFunc name expr scope') =
  lift $ evalStateT (eval expr) (Map.insert name x scope')
evalFuncVal x (VDelayed v)  = lift v >>= evalFuncVal x
evalFuncVal x (VNative _ n) = lift (n x)
evalFuncVal _ othr          =
  throwError $ TypeError $ show othr ++ " is not a function"
