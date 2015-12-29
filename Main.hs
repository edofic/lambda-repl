module Main where

import System.Console.Haskeline
import qualified Data.Map as Map
import qualified Eval
import Control.Monad.State
import qualified Parse


run :: String -> String
run source = either show show $ flip evalStateT Map.empty $ do
  ast <- Parse.parseExpr source
  Eval.eval ast

main :: IO ()
main = runInputT defaultSettings loop where
  loop = do
    sourceM <- getInputLine ">> "
    case sourceM of
      Just source -> outputStrLn (run source) >> loop
      Nothing     -> return ()
