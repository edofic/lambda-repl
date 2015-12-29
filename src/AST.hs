module AST where

data Expr = Ident String
          | Value Int
          | Lambda String Expr
          | Application Expr Expr
          | Let String Expr

instance Show Expr where
  show (Ident ident) = ident
  show (Value v) = show v
  show (Lambda name expr) = "λ" ++ name ++ "." ++ show expr
  show (Application e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (Let name expr) = "let " ++ name ++ " = " ++ show expr
