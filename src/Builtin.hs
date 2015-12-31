module Builtin where

import AST
import Control.Monad
import Types
import qualified Data.Map as Map


builtins :: Scope
builtins = Map.fromList [("plus", _plus)
                        ,("eq", _eq)
                        ,("Y", y)
                        ]

binary :: String -> (Value -> Value -> Either LambdaError Value) -> Value
binary name f = VNative name (whnf >=> \v1 ->
  return (VNative (name ++ "_1") (whnf >=> \v2 -> f v1 v2)))

y :: Value
y = VFunc "f" (Application
      (Lambda "x" (Application
        (Ident "f")
        (Application (Ident "x") (Ident "x"))))
      (Lambda "x" (Application
        (Ident "f")
        (Application (Ident "x") (Ident "x"))))) Map.empty


_plus :: Value
_plus = binary "plus" f where
  f (VInt n) (VInt m) = return $ VInt (n + m)

_eq :: Value
_eq = binary "eq" f where
  f a b = return $
    VFunc "a" (Lambda "b" $ Ident (if a == b then "a" else "b")) Map.empty

