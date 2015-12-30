import Test.Hspec

import AST
import Control.Monad
import Control.Monad.State
import Parse (parseExpr)
import Types
import qualified Data.Map as Map
import qualified Eval

eval :: Expr -> Either LambdaError Value
eval expr = evalStateT (Eval.eval expr) Map.empty

run :: String -> Either LambdaError Value
run s = runWith s Map.empty

runWith :: String -> Scope -> Either LambdaError Value
runWith s = evalStateT (parseExpr s >>= Eval.eval)

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parses identifiers" $
      parseExpr "foo" `shouldBe` Right (Ident "foo")
    it "parses constants" $
      parseExpr "1" `shouldBe` Right (Value 1)
    it "parses simple lambdas" $
      parseExpr "\\x.x" `shouldBe` Right (Lambda "x" $ Ident "x")
    it "parses nested lambdas" $
      parseExpr "\\f.\\x.f x" `shouldBe` Right (
        Lambda "f" $ Lambda "x" $
          Application (Ident "f") (Ident "x"))
    it "parses simple application" $
      parseExpr "f x" `shouldBe` Right (Application (Ident "f") (Ident "x"))
    it "parses multiple application" $
      parseExpr "a b c (d e) f" `shouldBe` Right (
        Application
          (Application
            (Application
              (Application (Ident "a") (Ident "b"))
              (Ident "c"))
            (Application (Ident "d") (Ident "e")))
          (Ident "f"))
    it "parses assignment" $
      parseExpr "let x = f 1" `shouldBe` Right (
        Let "x" (Application (Ident "f") (Value 1)))

  describe "Evaluator" $ do
    it "evaluates values" $ do
      eval (Value 1) `shouldBe` Right (VInt 1)
      run "1" `shouldBe` Right (VInt 1)
    it "looks up identifiers" $
      runWith "foo" (Map.insert "foo" (VInt 3) Map.empty) `shouldBe`
        Right (VInt 3)
    it "wraps lambdas" $ do
      let body  = Application (Ident "f") (Ident "x")
          fx    = Lambda "f" body
          fx'   = Application (Lambda "x" fx) (Value 1)
          scope = Map.fromList [("x", VDelayed (Right $ VInt 1))]
      eval fx `shouldBe` Right (VFunc "f" body Map.empty)
      eval fx' `shouldBe` Right (VFunc "f" body scope)
    it "allows assignment" $ do
      let program = ["let x = 1", "x"]
          comp = forM program (parseExpr >=> Eval.eval)
          res = evalStateT comp Map.empty
      res `shouldBe` Right [VInt 1, VInt 1]
    it "does simple application" $ 
      run "(\\x.x) 1" `shouldBe` Right (VDelayed $ Right $ VInt 1)
    it "does repeated application" $ do
      run "(\\x.x) (\\x.x) 1" `shouldBe` Right (VDelayed $ Right $ VInt 1)
      run "(\\f.\\x.f x) (\\x.x) 1" `shouldBe` Right (
        VDelayed $ Right $ VDelayed $ Right $ VInt 1)
