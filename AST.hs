module AST where

import Control.Monad.Error

data Expr = 
    Ident String 
  | Value Int
  | Lambda String Expr 
  | Application Expr Expr

instance Show Expr where
  show (Ident ident) = ident
  show (Value v) = show v
  show (Lambda name expr) = "λ" ++ name ++ "." ++ show expr
  show (Application e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"

data LambdaError = 
    ParsingError String
  | NotFound String 
  | TypeError String 
  | StrErr String deriving Show

instance Error LambdaError where
  noMsg = StrErr "Error"
  strMsg s = StrErr s

type LambdaMonad = Either LambdaError