{-# LANGUAGE LambdaCase #-}
module ParserBase where

import Control.Applicative
import Error
import Control.Monad
import Data.List

newtype Parser i a = Parser
  { runParser :: [i] -> Either [Error i] (a, [i])
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ \_ -> Left []

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

instance (Eq i) => MonadPlus (Parser i) where
  mzero = empty
  mplus = (<|>)

satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = Parser $ \case
  [] -> Left [EndOfInput]
  hd : rest
    | predicate hd -> Right (hd, rest)
    | otherwise -> Left [Unexpected hd]

match :: Eq i => i -> Parser i i
match i = satisfy (== i)