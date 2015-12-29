module Types where

data LambdaError = ParsingError String
                 | NotFound String
                 | TypeError String
                 | StrErr String deriving Show

type LambdaMonad = Either LambdaError
