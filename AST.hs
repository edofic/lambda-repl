module AST where

import Control.Monad.Error

data Expr = 
    Ident String 
  | Value Value
  | Lambda String Expr 
  | Application Expr Expr

instance Show Expr where
  show (Ident ident) = ident
  show (Value v) = show v
  show (Lambda name expr) = "λ" ++ name ++ "." ++ show expr
  show (Application e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"

data Value = 
    VInt Int
  | VFunc String Expr 
  
instance Show Value where
  show (VInt n) = show n
  show (VFunc name expr) = show $ Lambda name expr


data LambdaError = 
    ParsingError String
  | NotFound String
  | TypeError String 
  | StrErr String

instance Error LambdaError where
  noMsg = StrErr "Error"
  strMsg s = StrErr s

instance Show LambdaError where
  show (ParsingError s) = "Parsing error: " ++ s
  show (NotFound s) = "Not found: " ++ s
  show (TypeError s) = "Type error: " ++ s
  show (StrErr er) = "Error: " ++ er

type LambdaMonad = Either LambdaError