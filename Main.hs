module Main where

import AST
import Control.Monad (forever)
import System.Console.Haskeline
import Types
import qualified Data.Map as Map
import qualified Eval
import qualified Parse


parseExpr :: String -> LambdaMonad Expr
parseExpr input = case Parse.parseExpr input of
  Left er -> Left $ ParsingError $ show er
  Right ast -> Right ast

run :: String -> String
run source = either show show $ do
  ast <- parseExpr source
  Eval.eval Map.empty ast

main :: IO ()
main = runInputT defaultSettings loop where
  loop = do
    sourceM <- getInputLine ">> "
    case sourceM of
      Just source -> outputStrLn (run source) >> loop
      Nothing     -> return ()
