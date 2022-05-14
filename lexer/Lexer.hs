module Lexer where

import Control.Applicative
import Data.Char (isAlpha, isNumber, isSpace)
import Data.Functor
import ParserBase
import Token
import Error

stringToken :: Parser [Char] Token
stringToken = do
  _ <- match '"'
  text <- many (satisfy (/= '"'))
  _ <- match '"'
  return $ StringToken text

identifierToken :: Parser [Char] Token
identifierToken = IdentifierToken <$> some (satisfy isAlpha)

numberToken :: Parser [Char] Token
numberToken =
  do
    preDotVal <- some $ satisfy isNumber
    _ <- match '.'
    postDotVal <- some $ satisfy isNumber
    return $ NumberToken (read (preDotVal ++ "." ++ postDotVal))
    <|> NumberToken . read <$> some (satisfy isNumber)

reservedToken :: Parser [Char] Token
reservedToken =
  some (satisfy isSpace) $> ReservedToken WHITESPACE
    <|> match' "and" $> ReservedToken AND
    <|> match' "class" $> ReservedToken CLASS
    <|> match' "else" $> ReservedToken ELSE
    <|> match' "false" $> ReservedToken FALSE
    <|> match' "fun" $> ReservedToken FUN
    <|> match' "for" $> ReservedToken FOR
    <|> match' "if" $> ReservedToken IF
    <|> match' "nil" $> ReservedToken NIL
    <|> match' "or" $> ReservedToken OR
    <|> match' "print" $> ReservedToken PRINT
    <|> match' "return" $> ReservedToken RETURN
    <|> match' "super" $> ReservedToken SUPER
    <|> match' "this" $> ReservedToken THIS
    <|> match' "true" $> ReservedToken TRUE
    <|> match' "var" $> ReservedToken VAR
    <|> match' "while" $> ReservedToken WHILE
    <|> match' "!=" $> ReservedToken BANG_EQUAL
    <|> match' "==" $> ReservedToken EQUAL_EQUAL
    <|> match' ">=" $> ReservedToken GREATER_EQUAL
    <|> match' "<=" $> ReservedToken LESS_EQUAL
    <|> match '(' $> ReservedToken LEFT_PAREN
    <|> match ')' $> ReservedToken RIGHT_PAREN
    <|> match '[' $> ReservedToken LEFT_BRACE
    <|> match ']' $> ReservedToken RIGHT_BRACE
    <|> match ',' $> ReservedToken COMMA
    <|> match '.' $> ReservedToken DOT
    <|> match '-' $> ReservedToken MINUS
    <|> match '+' $> ReservedToken PLUS
    <|> match ';' $> ReservedToken SEMICOLON
    <|> match '/' $> ReservedToken SLASH
    <|> match '*' $> ReservedToken STAR
    <|> match '!' $> ReservedToken BANG
    <|> match '=' $> ReservedToken EQUAL
    <|> match '>' $> ReservedToken GREATER
    <|> match '<' $> ReservedToken LESS

stripWhitespace :: [Token] -> [Token]
stripWhitespace [] = []
stripWhitespace (ReservedToken WHITESPACE:xs) = stripWhitespace xs
stripWhitespace (x:xs) = x : stripWhitespace xs


lexString :: String -> Either [Error [Char]] [Token]
lexString s = case
    runParser
      (some $ reservedToken <|> numberToken <|> identifierToken
         <|> stringToken)
      s
  of
  Left ers -> Left ers
  Right (result, []) -> Right $ stripWhitespace result
  Right (_, x) -> Left [UnparsedData x]