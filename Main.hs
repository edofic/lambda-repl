{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Eval
import qualified Parse
import qualified Types
import System.Console.Haskeline


run :: MonadState Types.Scope m => String -> m String
run source = do
  scope <- get
  case runStateT (Parse.parseExpr source >>= Eval.eval) scope of
    Left err    -> return (show err)
    Right (v,s) -> put s >> return (show v)

loop :: StateT Types.Scope (InputT IO) ()
loop = do
    sourceM <- lift $ getInputLine ">> "
    case sourceM of
      Nothing  -> return ()
      Just src -> do
        res <- run src
        liftIO $ putStrLn res
        loop

main :: IO ()
main = runInputT defaultSettings $ evalStateT loop Map.empty
