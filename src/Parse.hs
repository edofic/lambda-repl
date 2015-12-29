module Parse where

import AST
import Text.ParserCombinators.Parsec

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "(unknown)"

expr = try application <|> nonApplication

nonApplication = lambda <|> value <|> ident <|> parened

parened = do
  string "("
  e <- expr
  string ")"
  return e

ident = Ident <$> many1 alphaNum

value = Value . read <$> many1 digit

lambda = do
  string "\\"
  Ident name <- ident
  string "."
  body <- expr
  return $ Lambda name body

application = do
  f <- nonApplication
  space
  xs <- sepBy1 nonApplication space
  return $ foldl Application f xs where
    space = many1 $ string " "


