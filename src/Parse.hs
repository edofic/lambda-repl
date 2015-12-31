{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse where

import AST
import Types
import Control.Monad.Except
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec

type P a = forall u . Parsec String u a

parseExpr :: MonadError LambdaError m => String -> m Expr
parseExpr s = case parse (expr <* eof) "(unknown)" s of
  Left er -> throwError $ ParsingError $ show er
  Right ast -> return ast


expr :: P Expr
expr = assignment <|> application

assignment :: P Expr
assignment = Let <$>
            (string "let " *> many1 alphaNum) <*>
            (string " = " *> expr)

application :: P Expr
application = foldl1 Application <$> sepBy1 nonApplication space

nonApplication :: P Expr
nonApplication = lambda <|> value <|> ident <|> parened

parened :: P Expr
parened = string "(" *> expr <* string ")"

ident :: P Expr
ident = Ident <$> many1 alphaNum

value :: P Expr
value = Value . read <$> ((++) <$> option "" (string "-") <*> many1 digit)

lambda :: P Expr
lambda = do
  Ident name <- string "\\" *> ident
  body <- string "." *> expr
  return $ Lambda name body
