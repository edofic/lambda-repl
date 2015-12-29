{-# LANGUAGE RankNTypes #-}

module Parse where

import AST
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec

type P a = forall u . Parsec String u a

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "(unknown)"

expr :: P Expr
expr = try application <|> nonApplication

nonApplication :: P Expr
nonApplication = lambda <|> value <|> ident <|> parened

parened :: P Expr
parened = do
  string "("
  e <- expr
  string ")"
  return e

ident :: P Expr
ident = Ident <$> many1 alphaNum

value :: P Expr
value = Value . read <$> many1 digit

lambda :: P Expr
lambda = do
  string "\\"
  Ident name <- ident
  string "."
  body <- expr
  return $ Lambda name body

application :: P Expr
application = do
  f <- nonApplication
  space
  xs <- sepBy1 nonApplication space
  return $ foldl Application f xs
