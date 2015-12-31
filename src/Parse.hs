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
expr = assignment <|> try application <|> nonApplication

assignment :: P Expr
assignment = Let <$>
            (string "let " *> many1 alphaNum) <*>
            (string " = " *> expr)

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

application :: P Expr
application = do
  f <- nonApplication <* space
  xs <- sepBy1 nonApplication space
  return $ foldl Application f xs
