module Eval where

import AST
import qualified Data.Map as Map
import Control.Monad.Error

type Scope = Map.Map String Value

maybeToEval :: String -> Maybe a -> LambdaMonad a
maybeToEval _ (Just a) = return a
maybeToEval name Nothing = throwError (NotFound name) 

eval :: Scope -> Expr -> LambdaMonad Value
eval _ (Value v) = return v
eval scope (Ident ident) = maybeToEval ident $ Map.lookup ident scope 
eval _ (Lambda name expr) = return $ VFunc name expr
eval scope (Application ef ex) = do
  f <- eval scope ef
  x <- eval scope ex
  case f of 
   (VFunc name expr) -> eval (Map.insert name x scope) expr
   othr -> throwError $ TypeError $ (show othr) ++ " is not a function"
  