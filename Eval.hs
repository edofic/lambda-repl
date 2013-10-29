module Eval where

import AST
import qualified Data.Map as Map
import Control.Monad.Error

type Scope = Map.Map String Value

data Value = 
    VInt Int
  | VFunc String Expr Scope
  | VDelayed (() -> LambdaMonad Value)
  
instance Show Value where
  show (VInt n) = show n
  show (VFunc name expr scope) = (show (Lambda name expr)) ++ " | " ++ show scope
  show (VDelayed v) = show $ v() 

maybeToEval :: String -> Maybe a -> LambdaMonad a
maybeToEval _ (Just a) = return a
maybeToEval name Nothing = throwError (NotFound name) 

eval :: Scope -> Expr -> () -> LambdaMonad Value
eval _ (Value v) _ = return $ VInt v
eval scope (Ident ident) _ = maybeToEval ident $ Map.lookup ident scope 
eval scope (Lambda name expr ) _ = return $ VFunc name expr scope
eval scope (Application f arg) _ = 
  let evalFuncVal x f = case f of 
       (VFunc name expr scope) -> (eval (Map.insert name x scope) expr)()
       (VDelayed v) -> v() >>=  evalFuncVal x
       othr -> throwError $ TypeError $ (show othr) ++ " is not a function" 
  in do
    f <- (eval scope f)()
    let x = VDelayed $ eval scope arg
    evalFuncVal x f
    