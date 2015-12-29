module Types where

import Control.Monad.State
import qualified Data.Map as Map
import AST

type Scope = Map.Map String Value

data Value = VInt Int
           | VFunc String Expr Scope
           | VDelayed (LambdaMonad Value)

instance Show Value where
  show (VInt n) = show n
  show (VFunc name expr scope) = show (Lambda name expr) ++ " | " ++ show scope
  show (VDelayed v) = case evalStateT v Map.empty of Left err -> show err
                                                     Right v' -> show v'

data LambdaError = ParsingError String
                 | NotFound String
                 | TypeError String
                 | StrErr String deriving Show

type LambdaMonad = StateT Scope (Either LambdaError)
