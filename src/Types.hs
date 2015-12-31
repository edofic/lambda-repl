module Types where

import Control.Monad.State
import qualified Data.Map as Map
import AST

type Scope = Map.Map String Value


data Value = VInt Int
           | VFunc String Expr Scope
           | VNative String (Value -> Either LambdaError Value)
           | VDelayed (Either LambdaError Value)

whnf :: Value -> Either LambdaError Value
whnf (VDelayed e) = e >>= whnf
whnf other = Right other

instance Eq Value where
  VInt n1 == VInt n2 = n1 == n2
  -- NOTE: function equality is code-wise
  --       equality on delayed *will* force
  VFunc n1 e1 s1 == VFunc n2 e2 s2 = n1 == n2 && e1 == e2 && s1 == s2
  -- NOTE: builtin equality is name-wise
  VNative n1 _ == VNative n2 _ = n1 == n2
  v1@(VDelayed _) == v2 = whnf v1 == whnf v2
  v1 == v2@(VDelayed _) = whnf v1 == whnf v2
  _ == _ = False

instance Show Value where
  show (VInt n) = show n
  show (VFunc name expr scope) = show (Lambda name expr) ++ " | " ++ show scope
  show (VNative name _) = "<builtin " ++ name ++ ">"
  show (VDelayed v) = case v of Left err -> show err
                                Right v' -> show v'

data LambdaError = ParsingError String
                 | NotFound String
                 | TypeError String
                 | StrErr String
                 deriving (Eq, Show)

type LambdaMonad = StateT Scope (Either LambdaError)
