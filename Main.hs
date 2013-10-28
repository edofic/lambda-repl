module Main where

import Control.Monad
import qualified Parse as Parse
import qualified Eval as Eval
import qualified Data.Map as Map
import AST

parseExpr :: String -> LambdaMonad Expr
parseExpr input = case Parse.parseExpr input of 
  Left er -> Left $ ParsingError $ show er
  Right ast -> Right ast

run :: String -> String 
run source = either show show $ do 
  ast <- parseExpr source
  (Eval.eval Map.empty ast)()

main = forever $ do 
  putStr ">> "
  source <- getLine
  putStrLn $ run source 